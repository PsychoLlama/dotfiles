" Get a human-friendly diff.
func! git#diff#Raw(query) abort
  let l:repo_root = git#repo#FindRoot(a:query.file)
  call assert#truthy(l:repo_root, 'Not inside a repo.')
  let l:cmd = 'git diff '

  " Diff from revision.
  if has_key(a:query, 'revision')
    let l:cmd .= a:query.revision . ' '
  endif

  let l:cmd .= '-- ' . fnameescape(a:query.file)

  let l:output = editor#util#ExecInDir(l:repo_root, l:cmd)
  call assert#truthy(!v:shell_error, 'git-diff failed.')

  return l:output
endfunc
