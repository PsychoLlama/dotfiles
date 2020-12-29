let s:copy = function('clippy#copy')
let s:paste = function('clippy#paste')

let g:clipboard = {
\   'name': 'clippy',
\   'cache_enabled': v:true,
\   'copy': {
\     '+': s:copy,
\     '*': s:copy,
\   },
\   'paste': {
\     '+': s:paste,
\     '*': s:paste,
\   },
\ }

nnoremap <silent> <Plug>(clippy-toggle-clipboard-mode) :call clippy#backend#toggle_clipboard_mode()<cr>
