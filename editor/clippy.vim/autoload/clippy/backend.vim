let s:TYPES = { 'VIRTUAL': 'virtual', 'HOST': 'host' }

let s:load_priority = [
\   {
\     'type': s:TYPES.VIRTUAL,
\     'load': function('clippy#backend#tmux#load'),
\   },
\   {
\     'type': s:TYPES.HOST,
\     'load': function('clippy#backend#macos#load'),
\   },
\   {
\     'type': s:TYPES.VIRTUAL,
\     'load': function('clippy#backend#memory#load'),
\   },
\ ]

" Proxy for g:clipboard supporting dynamic backends.
let s:backend = v:null
let s:backend_type = v:null

func! clippy#backend#() abort
  if s:backend is# v:null
    call s:set_backend(s:load_priority)
  endif

  return s:backend
endfunc

" The memory backend is always available. This should never fail.
func! clippy#backend#use_virtual() abort
  call s:set_backend(s:get_virtual_loaders())
endfunc

func! clippy#backend#use_host() abort
  if s:set_backend(s:get_host_loaders()) is# v:null
    call clippy#print#error('no supported host clipboards.')
  endif
endfunc

func! clippy#backend#toggle_clipboard_mode() abort
  if index([v:null, s:TYPES.HOST], s:backend_type) > 0
    call clippy#backend#use_virtual()
    call clippy#print#msg('using virtual clipboard.')
  else
    call clippy#backend#use_host()
    call clippy#print#msg('using host clipboard.')
  endif
endfunc

func! s:set_backend(loaders) abort
  let l:result = s:find_supported_loader(a:loaders)

  " No supported backend from the loader set.
  if l:result is# v:null
    return v:null
  endif

  let [s:backend, s:backend_type] = l:result

  return s:backend
endfunc

func! s:find_supported_loader(loaders) abort
  for l:loader in a:loaders
    let l:backend = l:loader.load()

    if l:backend isnot# v:null
      return [l:backend, l:loader.type]
    endif
  endfor

  return v:null
endfunc

func! s:get_virtual_loaders() abort
  let l:virtual_loaders = copy(s:load_priority)
  call filter(l:virtual_loaders, 'v:val.type is# s:TYPES.VIRTUAL')

  return l:virtual_loaders
endfunc

func! s:get_host_loaders() abort
  let l:host_loaders = copy(s:load_priority)
  call filter(l:host_loaders, 'v:val.type is# s:TYPES.HOST')

  return l:host_loaders
endfunc
