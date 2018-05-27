func! s:find_authors_for_range(start, end) abort
  let l:line_blames = git#blame#GetFileBlame({
        \   'ranges': [[a:start, a:end]],
        \   'file': expand('%:p'),
        \ })

  let l:authors = map(l:line_blames, { k, v -> v.author.name })
  let l:uniq_authors = {}

  for l:author in l:authors
    let l:uniq_authors[l:author] = l:author
  endfor

  return values(l:uniq_authors)
endfunc

func! s:PrintLineDetails(line) abort
  let [l:details] = git#blame#GetFileBlame({
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


" :<range>Author
func! editor#commands#Author(start, end) abort
  if &modified
    echo 'Save your changes first.'
    return
  endif

  " If there's only one selected line, show more details.
  if a:start == a:end
    return s:PrintLineDetails(a:start)
  endif

  let l:authors = s:find_authors_for_range(a:start, a:end)

  echo join(l:authors, "\n")

  let l:start_char = a:start == 0 || a:start == 1 ? '^' : a:start
  let l:end_char = a:end == line('$') ? '$' : a:end
  let l:range = l:start_char . ',' . l:end_char

  call editor#metrics#TrackEvent(':Author', { 'range': l:range })
endfunc


" :Node repl
func! editor#commands#Node() abort
  let l:project = editor#js#FindPackageRoot()

  new Node Repl
  wincmd J
  resize 10
  execute 'lcd ' . fnameescape(l:project)
  execute 'term node'
  normal! A

  call editor#metrics#TrackEvent(':Node', {})
endfunc


" :Reset (resets the file to HEAD state)
func! editor#commands#Reset() abort
  let l:file = fnameescape(expand('%:p'))
  let l:symlink_pointer = system('readlink ' . l:file)

  " Attempt to resolve symlinks.
  if len(l:symlink_pointer)
    let l:file = editor#util#chomp(l:symlink_pointer)
  endif

  " system(...) uses the cwd context. Not good if
  " you execute this from a different repo.
  cd! %:p:h
  call system('git reset -- ' . l:file)
  call system('git checkout -- ' . l:file)
  cd! -

  silent edit!
  silent write

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
  let l:filename = expand('%:t')
  let l:pane_name = l:filename . ' diff'

  lcd! %:p:h
  let l:diff_actual = systemlist('git diff HEAD -- ' . fnameescape(l:filename))
  lcd! -

  if v:shell_error
    echo 'Diff command failed.'
    return
  endif

  call editor#metrics#TrackEvent(':Diff', {})

  if len(l:diff_actual) == 0
    echo 'No local changes.'
    return
  endif

  execute 'new ' . l:pane_name
  wincmd J
  resize 20

  call editor#util#SetPaneContents(l:diff_actual)

  setfiletype diff
  setlocal buftype=nowrite bufhidden=delete signcolumn=no
  setlocal listchars= nomodifiable nowriteany nobuflisted nonumber
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
    echo editor#util#chomp(system('stat --format="%a" ' . l:file))
    return
  endif

  let l:output = system('chmod ' . a:1 . ' ' . l:file)

  if v:shell_error
    echoerr l:output
  endif
endfunc
