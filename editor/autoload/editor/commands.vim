" Order authors by quantity of lines written.
func! s:GetAuthorOwnership(authors, total)
  let l:result = []

  for l:author in keys(a:authors)
    let l:owned_lines = a:authors[l:author]
    let l:percentage = (l:owned_lines / a:total) * 100
    let l:result += [[l:author, float2nr(round(l:percentage))]]
  endfor

  call sort(l:result, {pair1, pair2 -> pair2[1] - pair1[1]})

  return l:result
endfunc

func! s:FindAuthorsForRange(start, end, all_commits) abort
  let l:line_blames = git#blame#({
        \   'include_all_commits': a:all_commits,
        \   'ranges': [[a:start, a:end]],
        \   'file': expand('%:p'),
        \ })

  let l:authors = map(l:line_blames, { k, v -> v.author.name })
  let l:uniq_authors = {}

  for l:author in l:authors
    let l:uniq_authors[l:author] = get(l:uniq_authors, l:author, 0) + 1
  endfor

  return s:GetAuthorOwnership(l:uniq_authors, a:end - a:start + 1.0)
endfunc

func! s:PrintLineDetails(line, all_commits) abort
  let [l:details] = git#blame#({
        \   'include_all_commits': a:all_commits,
        \   'ranges': [[a:line, a:line]],
        \   'file': expand('%:p'),
        \ })

  let l:date = strftime('%m/%d/%Y', l:details.author.time)
  echohl String
  echon l:details.sha[0:6]
  echohl Clear
  echon ': ' . l:details.author.name . ' ('
  echohl Type
  echon l:date
  echohl Clear
  echon ')'

  echo l:details.summary
endfunc


" :<range>Author[!]
func! editor#commands#Author(start, end, all_commits) abort
  if &modified
    echo 'Save your changes first.'
    return
  endif

  if isdirectory(expand('%:p'))
    echo 'Uh, this is a directory'
    return
  endif

  " If there's only one selected line, show more details.
  if a:start == a:end
    return s:PrintLineDetails(a:start, a:all_commits)
  endif

  for [l:author, l:ownership] in s:FindAuthorsForRange(a:start, a:end, a:all_commits)
    echohl Clear
    echo l:author
    echohl Comment
    echon ' (' . l:ownership . '%)'
    echohl Clear
  endfor

  let l:start_char = a:start == 0 || a:start == 1 ? '^' : a:start
  let l:end_char = a:end == line('$') ? '$' : a:end
  let l:range = l:start_char . ',' . l:end_char

  call editor#metrics#TrackEvent(':Author', { 'range': l:range })
endfunc


" :Node repl
func! editor#commands#Node() abort
  call assert#truthy(executable('node'), 'No node executable.')
  let l:project = editor#js#FindPackageRoot()

  new Node Repl
  wincmd J
  resize 10
  execute 'lcd ' . fnameescape(l:project)
  call termopen('node')
  normal! A

  call editor#metrics#TrackEvent(':Node', {})
endfunc


" :Reset (resets the file to HEAD state)
func! editor#commands#Reset() abort
  " Resolve any symlinks.
  let l:file = fnameescape(resolve(expand('%:p')))
  let l:view = winsaveview()

  " system(...) uses the cwd context. Not good if
  " you execute this from a different repo.
  cd! %:p:h
  call system('git reset -- ' . l:file)
  call system('git checkout -- ' . l:file)
  cd! -

  " Reload the file.
  silent edit!
  silent write

  " Calling `edit` changes the view position.
  call winrestview(l:view)

  echohl String
  echo 'Changes reverted.'
  echohl None

  call editor#metrics#TrackEvent(':Reset', {})
endfunc


" :Readme <module>
func! editor#commands#Readme(module) abort range
  let l:cmd = 'npm info ' . shellescape(a:module) . ' readme'
  let l:readme = systemlist(l:cmd)
  let l:readme = l:readme[1:len(l:readme) - 3]

  if v:shell_error
    echo 'Huh, you sure that module exists? (' . a:module . ')'
    return
  endif

  execute 'new ' . fnameescape(a:module) . ' (readme)'
  setfiletype markdown

  call editor#util#SetPaneContents(l:readme)

  setlocal buftype=nowrite bufhidden=delete signcolumn=no
  setlocal listchars= nomodifiable nowriteany nobuflisted

  call editor#metrics#TrackEvent(':Readme', { 'module': a:module })
endfunc


" :Diff
func! editor#commands#Diff() abort
  if &modified
    echo "Save 'em first."
    return
  endif

  let l:file = resolve(expand('%:p'))

  if !git#repo#IsFileTracked(l:file)
    echo 'Wait a minute, this file is untracked!'
    return
  endif

  let l:diff_actual = git#diff#Raw({ 'file': l:file, 'revision': 'HEAD' })

  if len(l:diff_actual) == 0
    echo 'No changes.'
    return
  endif

  let l:pane_name = 'diff (' . expand('%:t') . ')'
  execute '12 new ' . l:pane_name
  wincmd r

  call editor#util#SetPaneContents(l:diff_actual)
  setfiletype diff

  setlocal buftype=nowrite bufhidden=delete signcolumn=no
  setlocal listchars= nomodifiable nowriteany nobuflisted nonumber

  call editor#metrics#TrackEvent(':Diff', {})
endfunc


" :Z ...args
func! editor#commands#Z(...) abort
  let l:z_path = system('printf "$(dotfiles dir)/artifacts/z/z.sh"')
  let l:search = join(a:000, ' ')
  let l:cmd = 'source ' . fnameescape(l:z_path) . '; _z -l ' . shellescape(l:search)

  let l:matches = systemlist(l:cmd)
  if len(l:matches) is 0
    echo 'Nothing matches that description ("' . l:search . '").'
    return
  endif

  " Ignore the frecency metric, just pull the dirname.
  let l:directory = matchstr(l:matches[0], '\v/.*')
  execute 'edit ' . l:directory
endfunc


" :OpenTestFile
func! editor#commands#OpenTestFile() abort
  let l:test_file = editor#js#LocateTestFile()

  if l:test_file is v:null
    echo 'No test file.'
    return
  endif

  execute 'split ' . fnameescape(l:test_file)
endfunc


" :Perm +x
func! editor#commands#Permissions(...) abort
  let l:file_path = expand('%:p')
  let l:file = fnameescape(l:file_path)

  if !filereadable(l:file_path)
    echo 'Wait, you sure this is a file?'
    return
  endif

  if len(a:000) == 0
    echo getfperm(l:file)
    return
  endif

  let l:output = system('chmod ' . a:1 . ' ' . l:file)

  if v:shell_error
    echoerr l:output
  endif
endfunc
