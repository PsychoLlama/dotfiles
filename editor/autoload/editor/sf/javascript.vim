func! s:Scaffold(content, ...) abort
  let l:indent_level = get(a:000, 0, indent('.'))
  let l:indent_str = join(map(range(l:indent_level), "' '"), '')
  call filter(a:content, 'type(v:val) ==# v:t_string')
  return map(a:content, 'strlen(v:val) ? l:indent_str . v:val : ""')
endfunc

func! editor#sf#javascript#UnitTest() abort
  let l:content = s:Scaffold([
        \   getline('.') !~# 'describe' && '',
        \   "it('', () => {",
        \   '',
        \   '});',
        \ ])

  call append(line('.'), l:content)
  call cursor(line('.') + 2, 1)
  normal! f'
endfunc

func! editor#sf#javascript#TestGroup() abort
  let l:content = s:Scaffold([
        \   '',
        \   "describe('', () => {",
        \   '',
        \   '});',
        \ ])

  call append(line('.'), l:content)
  call cursor(line('.') + 2, 1)
  normal! f'
endfunc

func! editor#sf#javascript#LogStatement() abort
  let l:content = s:Scaffold([
        \   "console.log('');",
        \ ])

  call setline(line('.'), l:content)
  normal! ==
endfunc
