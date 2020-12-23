" Falsy values:
" - v:false
" - v:null
" - ''
" - 0
func! s:coerce_to_number(value) abort
  let l:type = type(a:value)

  if l:type == v:t_func || l:type == v:t_list || l:type == v:t_dict
    return 1
  elseif l:type == v:t_number
    return a:value
  elseif l:type == v:t_string
    return len(a:value)
  endif

  return a:value
endfunc

func! s:coerce_to_bool(value) abort
  return s:coerce_to_number(a:value) != 0 ? v:true : v:false
endfunc

" Throw an error if the value is falsy.
func! assert#(value, message) abort
  if !s:coerce_to_bool(a:value)
    throw a:message
  endif
endfunc

" Throw if the value isn't the expected type.
func! assert#type(value, type, message) abort
  if !has_key(v:, 't_' . a:type)
    throw 'Invalid type name ' . string(a:type)
  endif

  if type(a:value) != v:t_{a:type}
    throw a:message
  endif
endfunc
