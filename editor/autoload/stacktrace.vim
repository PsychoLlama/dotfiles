" The last point on the stack trace is strangely formatted.
" Turns 'stacktrace#Create, line 2' into 'stacktrace#Create[2]'
func! s:NormalizeLastStackTrace(frame) abort
  let l:funcname = substitute(a:frame, '\v,.*', '', '')
  let l:line_number = substitute(a:frame, '\v^.*, \D*', '', '')
  return l:funcname . '[' . l:line_number . ']'
endfunc

" Make the function name prettier.
func! s:NormalizeFuncRef(details) abort
  " Anonymous object method.
  if a:details.ref =~# '\v^\d+$'
    return '<anonymous>' . a:details.ref
  endif

  " <SNR>123_Bacon => s:Bacon
  if a:details.script_scoped
    return substitute(a:details.ref, '\v^\d+_', 's:', '')
  endif

  " Eh, we tried.
  return a:details.ref
endfunc

" 'example#fn[12]' => { 'ref': 'example#fn', 'line': 12 }
func! s:ParseFrame(frame) abort
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

  let l:result.name = s:NormalizeFuncRef(l:result)

  return l:result
endfunc

" Given some function details, expand its definition.
" Uses :function {name}
func! s:ExpandFunctionDefinition(func) abort
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
func! s:ParseFunctionLines(lines) abort
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
func! s:ParseFunctionDefinition(def) abort
  let l:output_lines = split(a:def, "\n")

  " The file name output is human optimized, not parser optimized.
  "     Last set from ~/some/path/to/file.vim
  let l:prefix = 'Last set from '
  let l:prefix_regex = '\c\v\s*' . l:prefix
  let l:file_output = l:output_lines[1]
  let l:filename = substitute(l:file_output, l:prefix_regex, '', '')

  let l:function_output = l:output_lines[0:0] + l:output_lines[2:]
  let l:function_source = s:ParseFunctionLines(l:function_output)

  return { 'source': l:function_source, 'file': expand(l:filename) }
endfunc

" Expands a function[line] expression with useful
" stack trace details.
func! s:ExpandFunction(frame) abort
  let l:func = s:ParseFrame(a:frame)
  let l:definition = s:ExpandFunctionDefinition(l:func)
  let l:func_details = s:ParseFunctionDefinition(l:definition)

  return extend(copy(l:func), l:func_details, 'error')
endfunc

" Pull more information about a v:throwpoint.
func! s:ExpandStackTrace(stacktrace) abort
  let l:result = range(len(a:stacktrace))
  let l:index = 0

  while l:index < len(a:stacktrace)
    let l:frame = a:stacktrace[l:index]
    let l:expansion = s:ExpandFunction(l:frame)
    let l:result[l:index] = l:expansion
    let l:index += 1
  endwhile

  return l:result
endfunc

" Example stack trace:
" function 148[6]..149[1]..<SNR>130_Script[6]..stacktrace#Create, line 2
func! stacktrace#Parse(throwpoint) abort
  let l:stack = substitute(a:throwpoint, '\v^function ', '', '')
  let l:functions = split(l:stack, '\V..')

  let l:functions[-1] = s:NormalizeLastStackTrace(l:functions[-1])
  return s:ExpandStackTrace(l:functions)
endfunc

" Get a new stacktrace.
func! stacktrace#Create() abort
  try
    throw 'Generating new stacktrace (stacktrace#Create)'
  catch
    return stacktrace#Parse(v:throwpoint)
  endtry
endfunc
