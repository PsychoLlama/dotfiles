func! editor#OpenRoot() abort
  if editor#js#IsJavaScript()
    return editor#js#OpenPackageRoot()
  endif

  let l:repo_root = git#repo#FindRoot()
  if l:repo_root isnot# v:null
    execute 'edit ' . fnameescape(l:repo_root)
    return
  endif

  echo "It doesn't look like you're in a package."
endfunc
