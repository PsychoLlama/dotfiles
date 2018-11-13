" Enable hard-wrap autoformatting.
PencilHard

setlocal textwidth=72
setlocal spell

if strlen(getline(1)) is# 0
  call append(1, '')
endif

" Present a git diff
func! s:OpenDiffPane() abort
  let s:mount_point = winwidth('.') >= 72 * 2 ? 'L' : 'J'

  vsplit Diff
  let b:is_diff_pane = v:true

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

  " Put focus back on the commit window.
  let s:focus_point = s:mount_point is# 'L' ? 'h' : 'k'
  execute 'wincmd ' . s:focus_point
endfunc

" Get every non-backgrounded buffer object.
func! s:GetActiveBuffers() abort
  let l:buffers = getbufinfo()
  let l:visible_buffers = filter(l:buffers, {index, buffer -> buffer.loaded})

  return l:visible_buffers
endfunc

" Whether one of the open buffers is the diff pane.
func! s:HasDiffPane() abort
  let l:buffers = getbufinfo()
  call filter(l:buffers, {index, buf -> get(buf.variables, 'is_diff_pane')})

  return len(l:buffers) > 0
endfunc

" :edit runs the file again. Don't show two panes.
if !s:HasDiffPane()
  call s:OpenDiffPane()
endif

func! s:CloseDiffIfLastWindow() abort
  if exists('b:is_diff_pane') && len(s:GetActiveBuffers()) is 1
    exit
  endif
endfunc

augroup CloseDiffIfLastWindow
  autocmd!
  autocmd BufEnter * call <SID>CloseDiffIfLastWindow()
augroup END
