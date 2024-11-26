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
