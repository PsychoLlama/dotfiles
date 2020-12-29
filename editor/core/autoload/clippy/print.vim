func! clippy#print#msg(text) abort
  echohl Function
  echon 'clippy:'
  echohl Clear
  echon ' ' a:text
endfunc

func! clippy#print#error(text) abort
  echohl Error
  echon 'clippy:'
  echohl Clear
  echon ' ' a:text
endfunc
