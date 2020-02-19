func! s:IsTypingWord() abort
  let l:col = col('.') - 1

  " Require at least 2 chars of context.
  if l:col < 3
    return 0
  endif

  let l:prev_chars = getline('.')[l:col - 2:l:col]
  return l:prev_chars =~? '\w\{2}'
endfunc

func! s:TabCompletion(shifting) abort
  if pumvisible()
    if a:shifting
      return "\<C-p>"
    endif

    return "\<C-n>"
  endif

  if s:IsTypingWord()
    return "\<C-n>"
  endif

  return "\t"
endfunc

func! s:ToggleCopyMode() abort
  if &number
    setlocal nonumber signcolumn=no
  else
    setlocal number signcolumn=yes
  endif
endfunc

func! s:EditVimrc() abort
  let l:cmd = isdirectory(expand('%:p')) ? 'edit' : 'tabedit'
  let l:editor_path = dotfiles#Path('editor/autoload')
  execute l:cmd . ' ' . l:editor_path
endfunc

" :Rexplore only works if the file was opened via netrw.
func! s:ExploreCurrentDir() abort
  if &filetype is# 'netrw'
    return
  endif

  let l:filename = expand('%:p:t')
  let l:curdir = expand('%:p:h')
  execute 'edit! ' . fnameescape(l:curdir)
  call search('\V\^' . l:filename . '\$')
endfunc

" Locating a source file from a test is harder. Remember
" what file we came from so we can get back.
let s:test_file_map = {}

" Open the test file if the source file is open (and vice versa).
func! s:AlternateTestFile() abort
  if &filetype !~# '\v(javascript|typescript)'
    echo &filetype "isn't supported."
    return
  endif

  let l:is_test_file = editor#js#IsTestFile()
  let l:current_file_path = expand('%:p')

  if l:is_test_file && has_key(s:test_file_map, l:current_file_path)
    call execute('edit ' . fnameescape(s:test_file_map[l:current_file_path]))
    return
  elseif l:is_test_file
    let l:src_file = editor#js#LocateSourceFile()

    if filereadable(l:src_file)
      call execute('edit ' . fnameescape(l:src_file))
    else
      echo 'So... this is awkward.'
      echo "I don't know where the source file is."
    endif

    return
  endif

  let l:test_file = editor#js#LocateTestFile()
  if l:test_file is# v:null
    echo "Can't be found."
    return
  endif

  let s:test_file_map[l:test_file] = l:current_file_path
  call execute('edit ' . fnameescape(l:test_file))
endfunc

inoremap <silent><expr><tab> <SID>TabCompletion(v:false)
inoremap <silent><expr><s-tab> <SID>TabCompletion(v:true)
nnoremap <silent><leader>t :call <SID>ToggleCopyMode()<cr>
nnoremap <silent><leader>v :call <SID>EditVimrc()<cr>
nnoremap <silent><leader>r :call <SID>ExploreCurrentDir()<cr>
nnoremap <silent><leader>a :call alternaut#Toggle()<cr>
nnoremap <silent><leader>p <esc>:call editor#OpenRoot()<cr>
nnoremap <silent><leader>; <esc>:call editor#commands#Test()<cr>
nnoremap <silent><esc> :nohlsearch<cr><esc>
nnoremap <leader>sc <esc>:call editor#sf#javascript#LogStatement()<cr>f'
nnoremap <leader>f <esc>:Files!<cr>
