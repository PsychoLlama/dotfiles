func! dotfiles#repo() abort
  let [l:path] = systemlist('dotfiles dir')

  return l:path
endfunc

func! dotfiles#path(relative) abort
  let l:dotfiles_dir = dotfiles#repo()
  let l:path = substitute(a:relative, '\v^/', '', '')

  return l:dotfiles_dir . '/' . l:path
endfunc
