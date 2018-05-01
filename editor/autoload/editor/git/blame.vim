" Parse the git blame for any given file.
"
" Format reference:
" https://git-scm.com/docs/git-blame#_the_porcelain_format

" Get the blame string output.
func! s:GetBlameOutput(file)
  let l:cmd = 'git blame --line-porcelain -- ' . fnameescape(a:file)

  return editor#util#ExecInDir(a:file, l:cmd)
endfunc

" Whether the line contains the given header.
func! s:HasHeader(header, line)
  let l:line_header = a:line[0:strlen(a:header) - 1]

  return l:line_header is# a:header
endfunc

" Remove the line header.
func! s:StripHeader(header, line)
  return a:line[strlen(a:header) + 1:]
endfunc

func! s:GetLineHeader(line)
  let l:delimiter_index = stridx(a:line, ' ')
  return a:line[0:l:delimiter_index - 1]
endfunc

let s:AUTHOR_TEMPLATE = {
      \   'name': v:null,
      \   'email': v:null,
      \   'time': v:null,
      \   'zone': v:null,
      \ }

let s:BLAME_TEMPLATE = {
      \   'line': { 'contents': v:null, 'number': v:null },
      \   'committer': deepcopy(s:AUTHOR_TEMPLATE),
      \   'author': deepcopy(s:AUTHOR_TEMPLATE),
      \   'prev_filename': v:null,
      \   'prev_sha': v:null,
      \   'summary': v:null,
      \   'sha': v:null,
      \ }

" The git sha marks the start of blame dictionaries.
" It will always be 40 characters.
func! s:IsHash(header)
  return strlen(a:header) is 40
endfunc

" Parse the sha & line numbers.
" <sha> <prev_lnum> <cur_lnum> ?<group_lnums>
func! s:AddShaDetails(line, blame)
  let l:parts = split(a:line, ' ')

  let a:blame.sha = l:parts[0]
  let a:blame.line.number = str2nr(l:parts[2])
endfunc

" Parse and inject the line contents.
func! s:AddLineContents(line, blame)
  let l:contents = substitute(a:line, '\v^\t', '', '')
  let a:blame.line.contents = l:contents
endfunc

let s:AUTHOR_NAME_MAP = {
      \   '': 'name',
      \   '-mail': 'email',
      \   '-time': 'time',
      \   '-tz': 'zone',
      \ }

" Add author/committer metadata.
func! s:AddAuthorDetails(header, line, author)
  let l:header_key = substitute(a:header, '\v^(author|committer)', '', '')
  let l:key = s:AUTHOR_NAME_MAP[l:header_key]
  let l:content = a:line[strlen(a:header) + 1:]

  " Trim the angle brackets from <name@email.domain>
  if l:key is# 'email'
    let l:content = l:content[1:-2]
  endif

  if l:key is# 'time'
    let l:content = str2nr(l:content)
  endif

  let a:author[l:key] = l:content
endfunc

" Contains information about the file prior to the commit.
" 'previous <sha> <filename>'
func! s:AddPrevShaDetails(header, line, blame)
  let l:value = s:StripHeader(a:header, a:line)
  let l:delimiter_index = stridx(l:value, ' ')

  let a:blame.prev_filename = l:value[(l:delimiter_index + 1):]
  let a:blame.prev_sha = l:value[0:l:delimiter_index - 1]
endfunc

" `summary` is the git commit title.
func! s:AddSummaryDetails(header, line, blame)
  let a:blame.summary = s:StripHeader(a:header, a:line)
endfunc

" string[] -> Blame[]
func! s:ParseBlameOutput(output)
  let l:line_blames = []

  for l:line in a:output
    let l:header = s:GetLineHeader(l:line)

    if s:IsHash(l:header)
      let l:blame = deepcopy(s:BLAME_TEMPLATE)
      call s:AddShaDetails(l:line, l:blame)
      let l:blame.sha = l:header

      call add(l:line_blames, l:blame)
    elseif l:line =~# '\v^\t'
      call s:AddLineContents(l:line, l:blame)
    elseif l:header =~# 'author'
      call s:AddAuthorDetails(l:header, l:line, l:blame.author)
    elseif l:header =~# 'committer'
      call s:AddAuthorDetails(l:header, l:line, l:blame.committer)
    elseif l:header is# 'previous'
      call s:AddPrevShaDetails(l:header, l:line, l:blame)
    elseif l:header is# 'summary'
      call s:AddSummaryDetails(l:header, l:line, l:blame)
    endif
  endfor

  return l:line_blames
endfunc

" Get a list of metadata for each line.
func! editor#git#blame#GetFileBlame(file)
  if !editor#git#util#ExistsInsideRepo(a:file)
    return v:null
  endif

  let l:output = s:GetBlameOutput(a:file)
  return s:ParseBlameOutput(l:output)
endfunc
