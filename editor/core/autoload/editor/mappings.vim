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
  if pumvisible()
    if a:shifting
      return "\<C-p>"
    endif

    return "\<C-n>"
  endif

  if s:is_typing_word()
    return "\<C-n>"
  endif

  return "\t"
endfunc

" :Rexplore only works if the file was opened via netrw.
func! editor#mappings#explore_current_dir() abort
  if &filetype is# 'netrw'
    return
  endif

  let l:filename = expand('%:p:t')
  let l:curdir = expand('%:p:h')
  execute 'edit! ' . fnameescape(l:curdir)
  call search('\V\^' . l:filename . '\$')
endfunc

func! editor#mappings#edit_vimrc() abort
  let l:cmd = isdirectory(expand('%:p')) ? 'edit' : 'tabedit'
  let l:editor_path = dotfiles#path('config/init.vim')
  execute l:cmd . ' ' . l:editor_path
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

  let l:runner = editor#js#get_test_command_for_path(l:file_path)
  let l:cmd = 'cd ' . fnameescape(l:runner.project) . '; '
  let l:cmd .= l:runner.command

  let l:tmux_vars = tmux#get_variables()
  if str2nr(l:tmux_vars.window_panes) < 2
    let l:test_pane = tmux#split_window({
          \   'horizontal': v:true,
          \   'percent': 45,
          \ })
    call tmux#send_keys(l:cmd, '^M')
  else
    let l:test_pane = l:tmux_vars.pane_at_right
    call tmux#select_pane(1)
    call tmux#send_keys('^C')
    call tmux#send_keys('^L', l:cmd, '^M')
  endif

  call tmux#select_pane(l:tmux_vars.pane_id)
endfunc
