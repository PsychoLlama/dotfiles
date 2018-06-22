" Construct the runtime environment.
let s:dotfiles_dir = systemlist('dotfiles dir')[0]
let s:editor_env = s:dotfiles_dir . '/editor'

call execute('set runtimepath+=' . s:editor_env)

" Each CLI argument has its own /tmp/ file.
func! s:GetCliArgs() abort
  let l:arg_count = str2nr($VIML_ARGV_COUNT)
  let l:file_prefix = $VIML_ARGV_FILE

  let l:args = []
  let l:arg_index = 0

  while l:arg_index < l:arg_count
    let l:filename = l:file_prefix . l:arg_index
    let l:arg = join(readfile(l:filename), "\n")
    call add(l:args, l:arg)
    let l:arg_index += 1
  endwhile

  " Make the file name absolute.
  let l:args[0] = simplify(getcwd() . '/' . l:args[0])

  return l:args
endfunc

" Execute the source file.
func s:ExecuteSourceFile() abort
  let l:argv = s:GetCliArgs()
  let g:process#argv = l:argv

  execute 'source ' . l:argv[0]
endfunc

let s:output = stacktrace#Capture(function('s:ExecuteSourceFile'))

" Determine exit code.
if s:output is g:stacktrace#Exception
  255 cquit
endif
