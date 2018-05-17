func! s:HasGitRepo(dir)
  let l:repo_folder = a:dir . '/.git'

  return isdirectory(l:repo_folder)
endfunc

" Is this directory or one of its parents a git repo?
func! git#repo#IsInsideRepo(target)
  let l:target = isdirectory(a:target) ? a:target : fnamemodify(a:target, ':h')
  let l:HasGitRepo = function('s:HasGitRepo')
  let l:result = editor#util#SearchDirUpwards(l:target, l:HasGitRepo)

  return l:result isnot v:null
endfunc
