" :SN -- Echo the (S)yntax (N)ame under the cursor.
func! editor#commands#SyntaxName() abort
  let l:syn_id = synID(line('.'), col('.'), v:true)
  let l:syn_name = synIDattr(l:syn_id, 'name')

  if empty(l:syn_name)
    echo 'No syntax ID. Lame.'
    return
  endif

  " Prints syntax metadata.
  execute 'highlight ' . l:syn_name
endfunc

" Order authors by quantity of lines written.
func! s:get_author_ownership(authors, total)
  let l:result = []

  for l:author in keys(a:authors)
    let l:owned_lines = a:authors[l:author]
    let l:percentage = (l:owned_lines / a:total) * 100
    let l:result += [[l:author, float2nr(round(l:percentage))]]
  endfor

  call sort(l:result, {pair1, pair2 -> pair2[1] - pair1[1]})

  return l:result
endfunc

func! s:find_authors_for_range(start, end, all_commits) abort
  let l:file = resolve(expand('%:p'))
  let l:line_blames = git#blame#({
        \   'include_all_commits': a:all_commits,
        \   'ranges': [[a:start, a:end]],
        \   'file': l:file,
        \ })

  let l:authors = map(l:line_blames, { k, v -> v.author.name })
  let l:uniq_authors = {}

  for l:author in l:authors
    let l:uniq_authors[l:author] = get(l:uniq_authors, l:author, 0) + 1
  endfor

  return s:get_author_ownership(l:uniq_authors, a:end - a:start + 1.0)
endfunc

func! s:print_line_details(line, all_commits) abort
  let [l:details] = git#blame#({
        \   'include_all_commits': a:all_commits,
        \   'ranges': [[a:line, a:line]],
        \   'file': expand('%:p'),
        \ })

  let l:date = strftime('%m/%d/%Y', l:details.author.time)
  echohl String
  echon l:details.sha[0:6]
  echohl Clear
  echon ': ' . l:details.author.name . ' ('
  echohl Type
  echon l:date
  echohl Clear
  echon ')'

  echo l:details.summary
endfunc


" :<range>Author[!]
func! editor#commands#Author(start, end, all_commits) abort
  if &modified
    echo 'Save your changes first.'
    return
  endif

  if isdirectory(expand('%:p'))
    echo 'Uh, this is a directory'
    return
  endif

  " If there's only one selected line, show more details.
  if a:start == a:end
    return s:print_line_details(a:start, a:all_commits)
  endif

  for [l:author, l:ownership] in s:find_authors_for_range(a:start, a:end, a:all_commits)
    echohl Clear
    echo l:author
    echohl Comment
    echon ' (' . l:ownership . '%)'
    echohl Clear
  endfor
endfunc


" :Node repl
func! editor#commands#Node() abort
  call assert#(executable('node'), 'No node executable.')
  let l:project = editor#js#FindPackageRoot()

  if l:project is# v:null
    let l:project = editor#util#ResolveDirectory()
  endif

  new Node Repl
  wincmd J
  resize 10
  execute 'lcd ' . fnameescape(l:project)
  call termopen('node')
  normal! A
endfunc


" :Perm +x
func! editor#commands#Permissions(...) abort
  let l:file_path = expand('%:p')
  let l:file = fnameescape(l:file_path)

  if !filereadable(l:file_path)
    echo 'Wait, you sure this is a file?'
    return
  endif

  if len(a:000) == 0
    echo getfperm(l:file)
    return
  endif

  let l:output = system('chmod ' . a:1 . ' ' . l:file)

  if v:shell_error
    echoerr l:output
  endif
endfunc


" :Test
func! editor#commands#Test() abort
  if &filetype !~# '\v(javascript|typescript)'
    echo 'WHAT ARE YOU DOING!'
    return
  endif

  let l:file_path = expand('%:p')
  if !alternaut#IsTestFile(l:file_path)
    let l:file_path = alternaut#LocateTestFile(l:file_path)
    if l:file_path is# v:null
      echohl Error
      echon 'Error:'
      echohl Clear
      echon " Couldn't find the test file."
      return
    endif
  endif

  let l:runner = editor#js#GetTestCommandForPath(l:file_path)

  " Allow external customization by overriding the bash command.
  if exists('*editor#env#GetTestShellCommand')
    " TODO: I broke this by porting `tw` to vimscript.
  endif

  let l:cmd = 'cd ' . fnameescape(l:runner.project) . '; '
  let l:cmd .= l:runner.command

  let l:tmux_vars = tmux#GetVariables()
  if str2nr(l:tmux_vars.window_panes) < 2
    let l:test_pane = tmux#SplitWindow({
          \   'horizontal': v:true,
          \   'percent': 45,
          \ })
    call tmux#SendKeys(l:cmd, '^M')
  else
    let l:test_pane = l:tmux_vars.pane_at_right
    call tmux#SelectPane(1)
    call tmux#SendKeys('^C')
    call tmux#SendKeys('^L', l:cmd, '^M')
  endif

  call tmux#SelectPane(l:tmux_vars.pane_id)
endfunc

func! editor#commands#Search(...) abort
  let l:args = len(a:000) ? a:000 : [getreg('"')]
  let l:search_term = join(l:args, ' ')
  let l:project_root = editor#js#FindPackageRoot()

  if l:project_root is# v:null
    let l:project_root = expand('%:p')
  endif

  if !isdirectory(l:project_root)
    let l:project_root = fnamemodify(l:project_root, ':h')
  endif

  execute 'silent lcd ' . fnameescape(l:project_root)
  execute 'silent grep ' . l:search_term
endfunc
