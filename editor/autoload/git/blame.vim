" Parse the git blame for any given file.
"
" Format reference:
" https://git-scm.com/docs/git-blame#_the_porcelain_format

" Get the blame string output.
func! s:GetBlameOutput(config)
  let l:cmd = 'git blame --line-porcelain '

  " Support for range restrictions (better performance).
  " For example: [[1, 2], [4, 5]] locates lines 1-2 & 4-5
  if has_key(a:config, 'ranges')
    let l:ranges = copy(a:config.ranges)
    call map(l:ranges, { i, range -> '-L ' . join(range, ',') })
    let l:cmd .= join(l:ranges, ' ') . ' '
  endif

  " Query the blame at a specific point in history.
  if has_key(a:config, 'revision')
    let l:cmd .= a:config.revision . ' '
  endif

  let l:cmd .= '-- ' . fnameescape(a:config.file)

  return editor#util#ExecInDir(a:config.file, l:cmd)
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

let s:LINE_TEMPLATE = {
      \   'prev_number': v:null,
      \   'contents': v:null,
      \   'number': v:null,
      \ }

let s:BLAME_TEMPLATE = {
      \   'committer': s:AUTHOR_TEMPLATE,
      \   'author': s:AUTHOR_TEMPLATE,
      \   'prev_filename': v:null,
      \   'line': s:LINE_TEMPLATE,
      \   'prev_sha': v:null,
      \   'summary': v:null,
      \   'sha': v:null,
      \ }

" The git sha marks the start of blame dictionaries.
" It will always be 40 characters.
func! s:IsHash(header)
  return len(a:header) is 40
endfunc

" Parse the sha & line numbers.
" <sha> <prev_lnum> <cur_lnum> ?<group_lnums>
func! s:AddShaDetails(line, blame)
  let l:parts = split(a:line, ' ')

  let a:blame.sha = l:parts[0]
  let a:blame.line.prev_number = str2nr(l:parts[1])
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
func! s:AddAuthorDetails(header, line, author, my_name)
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

  " If it's uncommitted, it's probably mine.
  if l:key is# 'name' && l:content =~? 'not committed yet'
    let l:content = a:my_name
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

func! s:GetUserName(file)
  " Get the current user's name.
  let l:containing_folder = fnamemodify(a:file, ':h')
  let l:cmd = 'git config user.name'
  let l:my_name = editor#util#ExecInDir(l:containing_folder, l:cmd)

  if !len(l:my_name)
    return '<current user>'
  endif

  return l:my_name[0]
endfunc

" string[] -> Blame[]
func! s:ParseBlameOutput(output, config)
  let l:my_name = s:GetUserName(a:config.file)
  let l:line_blames = []

  for l:line in a:output
    let l:header = s:GetLineHeader(l:line)

    if s:IsHash(l:header)
      let l:blame = deepcopy(s:BLAME_TEMPLATE)
      call s:AddShaDetails(l:line, l:blame)
      call add(l:line_blames, l:blame)
    elseif l:line =~# '\v^\t'
      call s:AddLineContents(l:line, l:blame)
    elseif l:header =~# 'author'
      call s:AddAuthorDetails(l:header, l:line, l:blame.author, l:my_name)
    elseif l:header =~# 'committer'
      call s:AddAuthorDetails(l:header, l:line, l:blame.committer, l:my_name)
    elseif l:header is# 'previous'
      call s:AddPrevShaDetails(l:header, l:line, l:blame)
    elseif l:header is# 'summary'
      call s:AddSummaryDetails(l:header, l:line, l:blame)
    endif
  endfor

  return l:line_blames
endfunc

" Turn the list of ignored commits to { [hash] => true }
" All commits are abbreviated to 7 characters.
func! s:GetIgnoredCommitMap() abort
  let l:ignored = get(g:, 'git#blame#ignored_commits', [])
  call assert#type(l:ignored, 'list', 'ignored_commits should be a list.')

  let l:map = {}
  for l:commit in l:ignored
    call assert#truthy(len(l:commit) >= 7, 'Hashes must be at least 7 characters.')

    let l:commit_abbrev = l:commit[0:6]
    let l:map[l:commit_abbrev] = v:true
  endfor

  return l:map
endfunc

" Replace blames when they exist in the prior set.
func! s:ReplaceIgnoredCommits(results, prior) abort
  let l:results = []

  for l:blame in a:results
    let l:deeper = get(a:prior, l:blame.prev_sha, {})
    let l:is_ignored = has_key(l:deeper, l:blame.line.prev_number)

    let l:commit = l:is_ignored ? l:deeper[l:blame.line.prev_number] : l:blame
    call add(l:results, l:commit)
  endfor

  return l:results
endfunc

" Resolve and index prior commits
func! s:ResolvePriorCommits(results, deeper_queries) abort
  let l:results = copy(a:results)
  let l:deeper_commits = {}

  " Query and index: { [revision] => { [line] => blame } }
  for l:query in values(a:deeper_queries)
    let l:result = git#blame#(l:query)
    let l:deeper_commits[l:query.revision] = {}

    for l:blame in l:result
      let l:deeper_commits[l:query.revision][l:blame.line.number] = l:blame
    endfor
  endfor

  return s:ReplaceIgnoredCommits(a:results, l:deeper_commits)
endfunc

" Check metadata for blacklisted commits.
func! s:DigDeeper(result) abort
  let l:repo_root = git#repo#FindRoot(a:result.input.file)
  let l:ignored = s:GetIgnoredCommitMap()
  let l:lookups = {} " { [hash]: <blame_config> }

  for l:blame in a:result.output
    " Only dig deeper for ignored commits with more history.
    let l:is_ignored = get(l:ignored, l:blame.sha[0:6], v:false)
    if !l:is_ignored || l:blame.prev_sha is# v:null
      continue
    endif

    let l:index = l:blame.prev_sha
    if !has_key(l:lookups, l:index)
      let l:file = l:repo_root . '/' . l:blame.prev_filename
      let l:lookups[l:index] = {}
      let l:lookups[l:index].file = l:file
      let l:lookups[l:index].revision = l:index
      let l:lookups[l:index].ranges = []
    endif

    let l:line = l:blame.line.prev_number
    call add(l:lookups[l:index].ranges, [l:line, l:line])
  endfor

  " No queried lines had ignored commits.
  if empty(l:lookups)
    return a:result.output
  endif

  return s:ResolvePriorCommits(a:result.output, l:lookups)
endfunc

" Get a list of metadata for each line.
func! git#blame#(blame)
  let l:revision = get(a:blame, 'revision', v:null)
  let l:track_config = { 'revision': l:revision }
  let l:is_tracked = git#repo#IsFileTracked(a:blame.file, l:track_config)

  call assert#truthy(l:is_tracked, "Can't git-blame an untracked file.")

  let l:output = s:GetBlameOutput(a:blame)
  let l:ownerships = s:ParseBlameOutput(l:output, a:blame)

  if get(a:blame, 'include_all_commits')
    return l:ownerships
  endif

  return s:DigDeeper({ 'output': l:ownerships, 'input': a:blame })
endfunc
