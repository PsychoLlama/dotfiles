func! tmux#get_variable(name) abort
  let l:cmd = "tmux display -p '#{" . shellescape(a:name) . "}'"
  return editor#util#chomp(system(l:cmd))
endfunc

func! tmux#split_window(...) abort
  let l:config = get(a:000, 0, {})
  let l:horizontal = get(l:config, 'horizontal', v:false)
  let l:percent = get(l:config, 'percent', 0)
  let l:cwd = get(l:config, 'cwd', '')
  let l:exec = get(l:config, 'exec', '')

  let l:cmd = 'tmux split-window'
  if l:horizontal
    let l:cmd .= ' -h'
  endif

  if l:percent > 0
    let l:cmd .= ' -p ' . l:percent
  endif

  if strlen(l:cwd) > 0
    let l:cmd .= ' -c ' . shellescape(l:cwd)
  endif

  if strlen(l:exec) > 0
    let l:cmd .= ' ' . l:exec
  endif

  call system(l:cmd)
  return system('tmux display -p "#{pane_id}"')
endfunc

func! tmux#send_keys(...) abort
  let l:keys = map(copy(a:000), { idx, str -> escape(str, '"') })
  let l:keys = map(l:keys, "'\"' . v:val . '\"'")
  let l:keys = join(l:keys, ' ')

  call system('tmux send-keys ' . l:keys)
endfunc

" By index (e.g. 1, 2, 3) or by ID (e.g. %20, %16)
func! tmux#select_pane(id) abort
  call system('tmux select-pane -t ' . a:id)
endfunc

func! tmux#kill_other_panes() abort
  let l:current_panel = tmux#get_variable('pane_id')
  call system('tmux kill-pane -a -t ' . shellescape(l:current_panel))
endfunc
