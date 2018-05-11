" :<range>Author
function! s:find_authors_for_range(start, end) abort
  let l:my_name = editor#util#chomp(system('git config user.name'))
  let l:line_blames = editor#git#blame#GetFileBlame({
        \   'ranges': [[a:start, a:end]],
        \   'file': expand('%:p'),
        \ })

  let l:authors = map(l:line_blames, { k, v -> v.author.name })
  let l:uniq_authors = {}

  for l:author in l:authors
    let l:actual_author = l:author ==? 'not committed yet' ? l:my_name : l:author
    let l:uniq_authors[l:actual_author] = l:actual_author
  endfor

  return values(l:uniq_authors)
endfunction

function! editor#commands#Author(start, end) abort
  if &modified
    echo 'Save your changes first.'
    return
  endif

  let l:authors = s:find_authors_for_range(a:start, a:end)

  echo join(l:authors, "\n")

  let l:start_char = a:start == 0 || a:start == 1 ? '^' : a:start
  let l:end_char = a:end == line('$') ? '$' : a:end
  let l:range = l:start_char . ',' . l:end_char

  call editor#metrics#TrackEvent(':Author', { 'range': l:range })
endfunction


" :Node repl
function! editor#commands#Node() abort
  let l:project = editor#js#FindPackageRoot()

  new Node Repl
  wincmd J
  resize 10
  execute 'lcd ' . fnameescape(l:project)
  execute 'term node'
  normal! A

  call editor#metrics#TrackEvent(':Node', {})
endfunction


" :Reset (resets the file to HEAD state)
function! editor#commands#Reset() abort
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
endfunction


" :Readme <module>
function! editor#commands#Readme(module) abort range
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
endfunction


" :Diff
function! editor#commands#Diff() abort
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
endfunction


" :Z ...args
function! editor#commands#Z(...) abort
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
endfunction


" :OpenTestFile
function! editor#commands#OpenTestFile() abort
  let l:test_file = editor#js#LocateTestFile()

  if l:test_file is v:null
    echo 'No test file.'
    return
  endif

  execute 'split ' . fnameescape(l:test_file)
endfunction
