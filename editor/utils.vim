

" :<range>Author
function! s:find_authors_for_range(start, end) abort
  let l:range = a:start . ',' . a:end
  let l:cmd = 'git blame --porcelain -L ' . l:range . ' -- ' . fnameescape(expand('%:p'))
  let l:my_name = editor#util#chomp(system('git config user.name'))

  cd! %:p:h
  let l:blames = systemlist(l:cmd)
  cd! -

  let l:committers = filter(l:blames, {k, v -> v =~# '^committer '})
  let l:authors = map(l:committers, {k, v -> substitute(v, 'committer ', '', '')})
  let l:uniq_authors = {}

  for l:author in l:authors
    let l:actual_author = l:author ==? 'not committed yet' ? l:my_name : l:author
    let l:uniq_authors[l:actual_author] = l:actual_author
  endfor

  return values(l:uniq_authors)
endfunction

function! s:find_line_author(start, end) abort
  let l:authors = s:find_authors_for_range(a:start, a:end)

  echo join(l:authors, "\n")

  let l:start_char = a:start == 0 || a:start == 1 ? '^' : a:start
  let l:end_char = a:end == line('$') ? '$' : a:end
  let l:range = l:start_char . ',' . l:end_char

  call editor#metrics#TrackEvent(':Author', { 'range': l:range })
endfunction

command! -range Author call <SID>find_line_author(<line1>, <line2>)


" :Node repl
function! s:open_node_repl() abort
  let l:project = editor#util#FindPackageRoot()

  new Node Repl
  wincmd J
  resize 10
  execute 'lcd ' . fnameescape(l:project)
  term node
  normal! A

  call editor#metrics#TrackEvent(':Node', {})
endfunction

command! Node call <SID>open_node_repl()


" :Reset (resets the file to HEAD state)
function! s:git_reset_file() abort
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

command! Reset call s:git_reset_file()


" :Readme <module>
function! s:open_package_readme(module) abort range
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

command! -nargs=1 Readme call <SID>open_package_readme(<f-args>)

" :Diff
function! s:show_file_diff() abort
  let l:filename = expand('%:t')
  let l:pane_name = l:filename . ' diff'

  lcd! %:p:h
  let l:diff_actual = systemlist('git diff -- ' . fnameescape(l:filename))
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

command! Diff call <SID>show_file_diff()

" :Z ...args
function! s:z_to_dir(...) abort
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

command! -nargs=+ Z call <SID>z_to_dir(<f-args>)
