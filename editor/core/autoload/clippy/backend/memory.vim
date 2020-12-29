let s:state = []

func! clippy#backend#memory#copy(lines, mode) abort
  let s:state = [a:lines, a:mode]
endfunc

func! clippy#backend#memory#paste() abort
  return s:state
endfunc

func! clippy#backend#memory#load() abort
  return {
  \   'copy': function('clippy#backend#memory#copy'),
  \   'paste': function('clippy#backend#memory#paste'),
  \ }
endfunc
