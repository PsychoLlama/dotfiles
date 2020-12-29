let g:dotfiles#env#directory = '~/dotfiles-env'

func! dotfiles#env#repo() abort
  let l:path = expand(g:dotfiles#env#directory)

  if !isdirectory(l:path)
    return v:null
  endif

  return l:path
endfunc

func! dotfiles#env#path(relative) abort
  let l:dotfiles_dir = dotfiles#env#repo()
  let l:path = substitute(a:relative, '\v^/', '', '')

  return l:dotfiles_dir . '/' . l:path
endfunc
