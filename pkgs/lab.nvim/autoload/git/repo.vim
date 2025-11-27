" Looks for directory/.git
func! s:has_git_folder(dir) abort
  let l:repo_folder = a:dir . '/.git'

  return isdirectory(l:repo_folder)
endfunc

" Find the root of the current repo directory.
" Optionally accepts a file path.
" Returns v:null if it isn't in a git repo.
func! git#repo#find_root(...) abort
  let l:directory = a:0 ? a:000[0] : expand('%:p')

  " Resolve the parent directory if given a filename.
  if filereadable(l:directory)
    let l:directory = fnamemodify(l:directory, ':h')
  endif

  return editor#util#search_dir_upwards(l:directory, funcref('s:has_git_folder'))
endfunc

" Is this file tracked?
func! git#repo#is_file_tracked(file, ...) abort
  let l:config = get(a:, 1, {})
  let l:revision = get(l:config, 'revision', v:null)

  let l:git_repo = git#repo#find_root(a:file)
  let l:repo_relative = substitute(a:file, l:git_repo, '', '')
  let l:rel_filename = simplify('./' . l:repo_relative)

  let l:cmd = 'git ls-files --error-unmatch '
  if l:revision isnot# v:null
    let l:cmd .= '--with-tree=' . l:revision . ' '
  endif
  let l:cmd .= '-- ' . l:rel_filename

  " cwd may not be in a git repo.
  call editor#util#exec_in_dir(l:git_repo, l:cmd)

  return v:shell_error ? v:false : v:true
endfunc
