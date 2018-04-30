" Present a git diff
let s:mount_point = winwidth('.') >= 72 * 2 ? 'L' : 'J'

vsplit Diff
let b:is_diff_window = v:true

execute 'wincmd ' . s:mount_point
setfiletype diff

let s:diff_lines = systemlist('git diff --staged')

" Show full diff for amended commits.
if len(s:diff_lines) == 0
  let s:diff_lines = systemlist('git diff HEAD^')
endif

call setline(1, s:diff_lines)

setlocal nomodifiable nowriteany nobuflisted nonumber listchars=tab:--
setlocal buftype=nowrite bufhidden=delete signcolumn=no
let s:focus_point = s:mount_point is# 'L' ? 'h' : 'k'
execute 'wincmd ' . s:focus_point

" Get every non-backgrounded buffer object.
function! s:get_active_buffers() abort
  let l:buffers = getbufinfo()
  let l:visible_buffers = filter(l:buffers, {index, buffer -> buffer.loaded})

  return l:visible_buffers
endfunction

function! s:close_diff_if_last_window() abort
  if exists('b:is_diff_window') && len(s:get_active_buffers()) is 1
    exit
  endif
endfunction

augroup close_diff_if_last_window
  autocmd!
  autocmd BufEnter * call <SID>close_diff_if_last_window()
augroup END
