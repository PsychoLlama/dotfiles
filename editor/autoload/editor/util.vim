" Trim dangling newlines. Mostly from system commands.
function! editor#util#chomp(string) abort
  return substitute(a:string, '\n$', '', '')
endfunction

" Replace buffer contents with a list of lines.
function! editor#util#SetPaneContents(lines) abort
  % delete

  let l:index = 0
  while l:index < len(a:lines)
    let l:line = a:lines[l:index]

    " Lines start at 1.
    call setline(l:index + 1, l:line)

    let l:index += 1
  endwhile
endfunction

" Search every parent directory until a predicate is satisfied.
function! editor#util#SearchDirUpwards(dir, cb) abort
  if a:cb(a:dir)
    return a:dir
  endif

  let l:dir = fnamemodify(a:dir, ':h')

  " Science has gone too far.
  if l:dir is a:dir
    return v:null
  endif

  return editor#util#SearchDirUpwards(l:dir, a:cb)
endfunction
