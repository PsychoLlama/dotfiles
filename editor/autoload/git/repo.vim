" Looks for directory/.git
func! s:HasGitFolder(dir)
  let l:repo_folder = a:dir . '/.git'

  return isdirectory(l:repo_folder)
endfunc

" Find the root of the current repo directory.
" Optionally accepts a file path.
" Returns v:null if it isn't in a git repo.
func! git#repo#FindRoot(...) abort
  let l:directory = a:0 ? a:000[0] : expand('%:p')

  " Resolve the parent directory if given a filename.
  if filereadable(l:directory)
    let l:directory = fnamemodify(l:directory, ':h')
  endif

  " Make sure the target actually exists.
  if !isdirectory(l:directory)
    return v:null
  endif

  let l:HasGitFolder = funcref('s:HasGitFolder')
  return editor#util#SearchDirUpwards(l:directory, l:HasGitFolder)
endfunc

" Is this directory or one of its parents a git repo?
func! git#repo#IsInsideRepo(...)
  let l:root = funcref('git#repo#FindRoot', a:000)()
  return l:root isnot v:null
endfunc