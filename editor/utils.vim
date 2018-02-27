let s:chomp = g:llama.utils.chomp

let g:llama.metrics = {
      \   'filename': expand('~/.vim/metrics.json'),
      \   'default_state': {
      \     'mappings': {},
      \     'events': {},
      \   },
      \ }

let s:metrics = g:llama.metrics

function! s:metrics.Read() abort dict
  if !filereadable(l:self.filename)
    call writefile([json_encode(l:self.default_state)], l:self.filename)
  endif

  let l:contents = readfile(l:self.filename)

  return json_decode(l:contents)
endfunction

function! s:metrics.Write(metrics) abort dict
  let l:contents = json_encode(a:metrics)

  call writefile([l:contents], l:self.filename)
endfunction

function! s:metrics.TrackEvent(event_name, metadata) abort dict
  let l:metrics = l:self.Read()

  if !has_key(l:metrics.events, a:event_name)
    let l:metrics.events[a:event_name] = []
  endif

  let l:metric = extend({ 'time': localtime() }, a:metadata, 'error')
  let l:metrics.events[a:event_name] += [l:metric]

  call l:self.Write(l:metrics)
endfunction


" Replace buffer contents with a list of lines.
function! g:llama.utils.SetPaneContents(lines) abort
  % delete

  let l:index = 0
  while l:index < len(a:lines)
    let l:line = a:lines[l:index]
    call setline(l:index, l:line)

    let l:index += 1
  endwhile
endfunction


" Search every parent directory until a predicate is satisfied.
function! g:llama.utils.SearchDirUpwards(dir, cb) abort
  if a:cb(a:dir)
    return a:dir
  endif

  let l:dir = fnamemodify(a:dir, ':h')

  " Science has gone too far.
  if l:dir is a:dir
    return v:null
  endif

  return g:llama.utils.SearchDirUpwards(l:dir, a:cb)
endfunction

" Locate the directory defining package.json.
function! g:llama.utils.FindProjectRoot() abort
  let l:current_dir = expand('%:p:h')
  let l:Has_pkg_json = {dir -> file_readable(dir . '/package.json')}
  let l:project = g:llama.utils.SearchDirUpwards(l:current_dir, l:Has_pkg_json)

  if l:project is v:null
    let l:project = l:current_dir
  endif

  return l:project
endfunction


" :<range>Author
function! s:find_authors_for_range(start, end) abort
  let l:range = a:start . ',' . a:end
  let l:cmd = 'git blame --porcelain -L ' . l:range . ' -- ' . fnameescape(expand('%:p'))
  let l:my_name = s:chomp(system('git config user.name'))

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

function! s:find_line_author() abort range
  let l:authors = s:find_authors_for_range(a:firstline, a:lastline)

  echo join(l:authors, "\n")

  let l:start_char = a:firstline == 0 || a:firstline == 1 ? '^' : a:firstline
  let l:end_char = a:lastline == line('$') ? '$' : a:lastline
  let l:range = l:start_char . ',' . l:end_char

  call s:metrics.TrackEvent(':Author', { 'range': l:range })
endfunction

command! -range Author <line1>,<line2>call <SID>find_line_author()


" :Node repl
function! s:open_node_repl() abort
  let l:project = g:llama.utils.FindProjectRoot()

  new Node Repl
  wincmd J
  resize 10
  execute 'lcd ' . fnameescape(l:project)
  term node
  normal! A

  call s:metrics.TrackEvent(':Node', {})
endfunction

command! Node call <SID>open_node_repl()


" :Reset (resets the file to HEAD state)
function! s:git_reset_file() abort
  let l:file = fnameescape(expand('%:p'))
  let l:symlink_pointer = system('readlink ' . l:file)

  " Attempt to resolve symlinks.
  if len(l:symlink_pointer)
    let l:file = s:chomp(l:symlink_pointer)
  endif

  " system(...) uses the cwd context. Not good if
  " you execute this from a different repo.
  cd! %:p:h
  call system('git reset -- ' . l:file)
  call system('git checkout -- ' . l:file)
  cd! -

  silent edit!
  silent write

  call s:metrics.TrackEvent(':Reset', {})
endfunction

command! Reset call s:git_reset_file()


" :Readme <module>
function! s:open_package_readme(module) abort range
  let l:cmd = 'npm info ' . shellescape(a:module) . ' readme'
  let l:readme = systemlist(l:cmd)
  let l:readme = l:readme[:len(l:readme) - 3]

  if v:shell_error
    echo 'Huh, you sure that module exists? (' . a:module . ')'
    return
  endif

  execute 'new ' . fnameescape(a:module) . ' (readme)'
  setfiletype markdown

  call g:llama.utils.SetPaneContents(l:readme)

  setlocal buftype=nowrite bufhidden=delete signcolumn=no
  setlocal listchars= nomodifiable nowriteany nobuflisted

  call s:metrics.TrackEvent(':Readme', { 'module': a:module })
endfunction

command! -nargs=1 Readme call <SID>open_package_readme(<f-args>)

" :Diff
function! s:show_file_diff() abort
  let l:filename = expand('%:t')
  let l:pane_name = l:filename . ' diff'

  lcd! %:p:h
  let l:diff_actual = systemlist('git diff -- ' . fnameescape(l:filename))
  lcd! -

  call s:metrics.TrackEvent(':Diff', {})

  if len(l:diff_actual) == 0
    echo 'No local changes.'
    return
  endif

  execute 'new ' . l:pane_name
  wincmd J
  resize 20

  call g:llama.utils.SetPaneContents(l:diff_actual)

  setfiletype diff
  setlocal buftype=nowrite bufhidden=delete signcolumn=no
  setlocal listchars= nomodifiable nowriteany nobuflisted nonumber
endfunction

command! Diff call <SID>show_file_diff()
