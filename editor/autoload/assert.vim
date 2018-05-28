" Falsy values:
" - v:false
" - v:null
" - ''
" - 0
func! s:CoerceToNumber(value) abort
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

func! s:CoerceToBool(value) abort
  return s:CoerceToNumber(a:value) ? v:true : v:false
endfunc

" Throw an error if the value is falsy.
func! assert#truthy(value, message) abort
  if !s:CoerceToBool(a:value)
    throw a:message
  endif
endfunc
