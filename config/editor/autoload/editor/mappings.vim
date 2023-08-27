func! s:is_typing_word() abort
  let l:col = col('.') - 1

  " Require at least 2 chars of context.
  if l:col < 3
    return 0
  endif

  let l:prev_chars = getline('.')[l:col - 2:l:col]
  return l:prev_chars =~? '\w\{2}'
endfunc

func! editor#mappings#tab_completion(shifting) abort
  " Tabbed while completion was active.
  if coc#pum#visible()
    if a:shifting
      return coc#pum#prev(1)
    endif

    return coc#pum#next(1)
  endif

  " The completion menu isn't open, but there's enough context for it.
  if s:is_typing_word()
    return coc#refresh()
  endif

  " You probably just wanted a regular "\t" character.
  return "\<Tab>"
endfunc

" :Rexplore only works if the file was opened via netrw.
func! editor#mappings#explore_current_dir() abort
  if &filetype is# 'netrw' || &filetype is# 'navitron'
    return
  endif

  let l:filename = expand('%:p:t')
  let l:curdir = expand('%:p:h')
  execute 'edit ' . fnameescape(l:curdir)
  call search('\V\^' . l:filename . '\$')
endfunc

func! editor#mappings#test() abort
  if &filetype !~# '\v(javascript|typescript)'
    echo 'WHAT ARE YOU DOING!'
    return
  endif

  let l:file_path = expand('%:p')
  if !alternaut#is_test_file(l:file_path)
    let l:file_path = alternaut#locate_test_file(l:file_path)
    if l:file_path is# v:null
      echohl Error
      echon 'Error:'
      echohl Clear
      echon " Couldn't find the test file."
      return
    endif
  endif

  if str2nr(tmux#get_variable('window_panes')) > 1
    call tmux#kill_other_panes()
  endif

  let l:runner = editor#js#get_test_command_for_path(l:file_path)
  let l:cmd = l:runner.command
  let l:code_pane = tmux#get_variable('pane_id')
  let l:test_pane = tmux#split_window({
        \   'horizontal': v:true,
        \   'percent': 45,
        \   'cwd': l:runner.project,
        \   'exec': 'nix develop --command ' . l:runner.command,
        \ })

  call tmux#select_pane(l:code_pane)
endfunc
