let s:mode = 'v'

func! clippy#backend#tmux#copy(lines, mode) abort
  let s:mode = a:mode
  call system('tmux load-buffer - &', a:lines)
endfunc

func! clippy#backend#tmux#paste() abort
  return [systemlist('tmux save-buffer -'), s:mode]
endfunc

func! clippy#backend#tmux#load() abort
  if empty($TMUX)
    return v:null
  endif

  return {
  \   'copy': function('clippy#backend#tmux#copy'),
  \   'paste': function('clippy#backend#tmux#paste'),
  \ }
endfunc
