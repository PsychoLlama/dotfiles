let g:dotfiles#env#directory = '~/dotfiles-env'

func! dotfiles#env#Repo() abort
  let l:path = expand(g:dotfiles#env#directory)

  if !isdirectory(l:path)
    return v:null
  endif

  return l:path
endfunc

func! dotfiles#env#Path(relative) abort
  let l:dotfiles_dir = dotfiles#env#Repo()
  let l:path = substitute(a:relative, '\v^/', '', '')

  return l:dotfiles_dir . '/' . l:path
endfunc
