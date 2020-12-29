let s:mode = 'v'

func! clippy#backend#macos#copy(lines, mode) abort
  let s:mode = a:mode
  call system('pbcopy &', a:lines)
endfunc

func! clippy#backend#macos#paste() abort
  return [systemlist('pbpaste'), s:mode]
endfunc

func! clippy#backend#macos#load() abort
  if !has('mac')
    return v:null
  endif

  return {
  \   'copy': function('clippy#backend#macos#copy'),
  \   'paste': function('clippy#backend#macos#paste'),
  \ }
endfunc
