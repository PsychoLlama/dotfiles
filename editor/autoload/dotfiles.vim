func! dotfiles#Repo() abort
  let [l:path] = systemlist('dotfiles dir')

  return l:path
endfunc

func! dotfiles#Path(relative) abort
  let l:dotfiles_dir = dotfiles#Repo()
  let l:path = substitute(a:relative, '\v^/', '', '')

  return l:dotfiles_dir . '/' . l:path
endfunc
