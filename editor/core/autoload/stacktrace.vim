" The last point on the stack trace is strangely formatted.
" Turns 'stacktrace#create, line 2' into 'stacktrace#create[2]'
func! s:normalize_last_stack_trace(frame) abort
  let l:funcname = substitute(a:frame, '\v,.*', '', '')
  let l:line_number = substitute(a:frame, '\v^.*, \D*', '', '')
  return l:funcname . '[' . l:line_number . ']'
endfunc

" Make the function name prettier.
func! s:normalize_funcref(details) abort
  " Anonymous object method.
  if a:details.ref =~# '\v^\d+$'
    return '<anonymous>' . a:details.ref
  endif

  " <SNR>123_bacon => s:bacon
  if a:details.script_scoped
    return substitute(a:details.ref, '\v^\d+_', 's:', '')
  endif

  " Eh, we tried.
  return a:details.ref
endfunc

" 'example#fn[12]' => { 'ref': 'example#fn', 'line': 12 }
func! s:parse_frame(frame) abort
  let l:matches = matchlist(a:frame, '\v^(.*)\[(\d+)\]')

  let l:result = {}
  let l:result.line = str2nr(l:matches[2])
  let l:result.ref = l:matches[1]

  let l:script_scoped = l:result.ref =~# '<SNR>'
  let l:result.script_scoped = l:script_scoped ? v:true : v:false

  " function {'<SNR>...'} doesn't respond well.
  if l:result.script_scoped
    let l:result.ref = substitute(l:result.ref, '^<SNR>', '', '')
  endif

  let l:result.name = s:normalize_funcref(l:result)

  return l:result
endfunc

" Given some function details, expand its definition.
" Uses :function {name}
func! s:expand_function_definition(func) abort
  let l:description = ''
  let l:expr = '{' . string(a:func.ref) . '}'
  if a:func.script_scoped
    let l:expr = '<SNR>' . l:expr
  endif

  " Expand the function definition. Ignore any stylistic
  " characters (yeah, those are actually displayed).
  let l:listchars = &listchars
  let &listchars = ''
  let l:output = execute('silent verbose function ' . l:expr)
  let &listchars = l:listchars

  return l:output
endfunc

" Parse out the source of a function definition.
" Most of this is trimming out line numbers.
func! s:parse_function_lines(lines) abort
  let l:source = range(len(a:lines))
  let l:default_offset = 3
  let l:index = 1

  " The first and last lines have no line numbers.
  while l:index < len(a:lines) - 1
    let l:line = a:lines[l:index]

    " Find the line number. Will fail if the first character
    " is a number in a >= 100 line function without indentation.
    let l:lnum = matchstr(l:line, '\v^\d+')
    let l:lnum_offset = len(l:lnum)
    let l:offset = max([l:lnum_offset, l:default_offset])
    let l:trimmed = l:line[(l:offset):]

    " Empty lines & lines with a single space look identical.
    if l:trimmed is# ' '
      let l:trimmed = ''
    endif

    let l:source[l:index] = l:trimmed
    let l:index += 1
  endwhile

  " Add the `function` and `endfunction` parts.
  let l:source[0] = a:lines[0][(l:default_offset):]
  let l:source[-1] = a:lines[-1][(l:default_offset):]

  return l:source
endfunc

" Extract lines & file definition from function {...} output
func! s:parse_function_definition(def) abort
  let l:output_lines = split(a:def, "\n")

  " The file name output is human optimized, not parser optimized.
  "     Last set from ~/some/path/to/file.vim
  let l:prefix = 'Last set from '
  let l:prefix_regex = '\c\v\s*' . l:prefix
  let l:file_output = l:output_lines[1]
  let l:filename = substitute(l:file_output, l:prefix_regex, '', '')

  let l:function_output = l:output_lines[0:0] + l:output_lines[2:]
  let l:function_source = s:parse_function_lines(l:function_output)

  return { 'source': l:function_source, 'file': expand(l:filename) }
endfunc

" Expands a function[line] expression with useful
" stack trace details.
func! s:expand_function(frame) abort
  let l:func = s:parse_frame(a:frame)
  let l:definition = s:expand_function_definition(l:func)
  let l:func_details = s:parse_function_definition(l:definition)

  return extend(copy(l:func), l:func_details, 'error')
endfunc

" Pull more information about a v:throwpoint.
func! s:expand_stack_trace(stacktrace) abort
  let l:result = range(len(a:stacktrace))
  let l:index = 0

  while l:index < len(a:stacktrace)
    let l:frame = a:stacktrace[l:index]
    let l:expansion = s:expand_function(l:frame)
    let l:result[l:index] = l:expansion
    let l:index += 1
  endwhile

  return l:result
endfunc

" If thrown outside a function, the stacktrace is structured differently.
" /Users/overlord/dotfiles/editor/autoload/stacktrace.vim, line 179
func! s:generate_file_stacktrace(throwpoint) abort
  let l:matches = matchlist(a:throwpoint, '\v(.*), line (\d+)')
  let l:result = {}
  let l:result.file = l:matches[1]
  let l:result.line = str2nr(l:matches[2]) - 1
  let l:result.source = readfile(l:result.file)
  let l:result.ref = v:null
  let l:result.name = fnamemodify(l:result.file, ':t')

  return [l:result]
endfunc

" Example stack trace:
" function 148[6]..149[1]..<SNR>130_script[6]..stacktrace#create, line 2
func! stacktrace#parse(throwpoint) abort
  " Thrown outside a function.
  if a:throwpoint =~# '\v^/'
    return s:generate_file_stacktrace(a:throwpoint)
  endif

  let l:stack = substitute(a:throwpoint, '\v^function ', '', '')
  let l:functions = split(l:stack, '\V..')

  let l:functions[-1] = s:normalize_last_stack_trace(l:functions[-1])
  return s:expand_stack_trace(l:functions)
endfunc

" Get a new stacktrace.
func! stacktrace#create(message) abort
  try
    throw a:message
  catch
    return stacktrace#parse(v:throwpoint)
  endtry
endfunc

" Pretty-print a parsed stack trace.
func! stacktrace#print(stack, exception) abort
  echohl Error
  echon 'Error:'
  echohl Clear
  echon ' ' . a:exception

  for l:trace in reverse(copy(a:stack))
    echon "\n"
    let l:filename = fnamemodify(l:trace.file, ':.')
    echohl Comment
    echon l:filename
    echohl Clear
    echon ' in '
    echohl Function
    echon l:trace.name
    echohl Comment
    echon ' (line '
    echohl Function
    echon l:trace.line
    echohl Comment
    echon ")\n"

    echo l:trace.source[l:trace.line] . "\n"
  endfor

  echohl Clear
endfunc

" Returned if the function threw. Errors are not re-emitted.
" Use this return value to determine if the wrapped function threw.
" `l:retval isnot# g:stacktrace#exception`
let g:stacktrace#exception = { '__stacktrace#exception': v:true }

" Captures thrown exceptions, analyzing & printing them.
func! stacktrace#capture(capture_target) abort
  try
    return a:capture_target()
  catch
    let l:trace = stacktrace#parse(v:throwpoint)
    call stacktrace#print(l:trace, v:exception)
    return g:stacktrace#exception
  endtry
endfunc
