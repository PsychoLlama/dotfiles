func! s:is_typing_word() abort
  let l:col = col('.') - 1

  " Require at least 2 chars of context.
  if l:col < 3
    return 0
  endif

  let l:prev_chars = getline('.')[l:col - 2:l:col]
  return l:prev_chars =~? '\w\{2}'
endfunc

func! s:tab_completion(shifting) abort
  if pumvisible()
    if a:shifting
      return "\<C-p>"
    endif

    return "\<C-n>"
  endif

  if s:is_typing_word()
    return "\<C-n>"
  endif

  return "\t"
endfunc

func! s:toggle_copy_mode() abort
  if &number
    setlocal nonumber signcolumn=no
  else
    setlocal number signcolumn=yes
  endif
endfunc

func! s:edit_vimrc() abort
  let l:cmd = isdirectory(expand('%:p')) ? 'edit' : 'tabedit'
  let l:editor_path = dotfiles#Path('editor/autoload')
  execute l:cmd . ' ' . l:editor_path
endfunc

" :Rexplore only works if the file was opened via netrw.
func! s:explore_current_dir() abort
  if &filetype is# 'netrw'
    return
  endif

  let l:filename = expand('%:p:t')
  let l:curdir = expand('%:p:h')
  execute 'edit! ' . fnameescape(l:curdir)
  call search(l:filename)
endfunc

" Locating a source file from a test is harder. Remember
" what file we came from so we can get back.
let s:test_file_map = {}

" Open the test file if the source file is open (and vice versa).
func! s:AlternateTestFile() abort
  if &filetype !~# 'javascript'
    echo &filetype "isn't supported."
    return
  endif

  let l:is_test_file = editor#js#IsTestFile()
  let l:current_file_path = expand('%:p')

  if l:is_test_file && has_key(s:test_file_map, l:current_file_path)
    call execute('edit ' . fnameescape(s:test_file_map[l:current_file_path]))
    return
  elseif l:is_test_file
    echo 'So... this is awkward.'
    echo "I don't know where the source file is."
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

inoremap <silent><expr><TAB> <SID>tab_completion(0)
inoremap <silent><expr><S-TAB> <SID>tab_completion(1)
nnoremap <silent><leader>t :call <SID>toggle_copy_mode()<cr>
nnoremap <silent><leader>n :nohlsearch<cr>
nnoremap <silent><leader>v :call <SID>edit_vimrc()<cr>
nnoremap <silent><leader>r :call <SID>explore_current_dir()<cr>
nnoremap <silent><leader>r :call <SID>explore_current_dir()<cr>
nnoremap <silent><leader>a :call <SID>AlternateTestFile()<cr>
