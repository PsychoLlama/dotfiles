func! editor#open_project_root() abort
  if editor#js#is_javascript()
    return editor#js#open_package_root()
  endif

  let l:repo_root = git#repo#find_root()
  if l:repo_root isnot# v:null
    execute 'edit ' . fnameescape(l:repo_root)
    return
  endif

  echo "It doesn't look like you're in a package."
endfunc
