func! s:scaffold(content, ...) abort
  let l:indent_level = get(a:000, 0, indent('.'))
  let l:indent_str = join(map(range(l:indent_level), "' '"), '')
  call filter(a:content, 'type(v:val) ==# v:t_string')
  return map(a:content, 'strlen(v:val) ? l:indent_str . v:val : ""')
endfunc

" TODO: Replace this trash with a real snippet engine.
func! editor#sf#javascript#log_statement() abort
  let l:content = s:scaffold([
        \   "console.log('');",
        \ ])

  call setline(line('.'), l:content)
  normal! ==
endfunc
