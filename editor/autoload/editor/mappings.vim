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

let g:mapleader = "\<space>"

inoremap <silent><expr><TAB> <SID>tab_completion(0)
inoremap <silent><expr><S-TAB> <SID>tab_completion(1)
nnoremap <silent><leader>t :call <SID>toggle_copy_mode()<cr>
nnoremap <silent><leader>n :nohlsearch<cr>
nnoremap <silent><leader>v :call <SID>edit_vimrc()<cr>
nnoremap <silent><leader>r :call <SID>explore_current_dir()<cr>
