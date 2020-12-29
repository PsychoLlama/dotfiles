func! clippy#copy(lines, mode) abort
  let l:backend = clippy#backend#()
  call l:backend.copy(a:lines, a:mode)
endfunc

func! clippy#paste() abort
  return clippy#backend#().paste()
endfunc
