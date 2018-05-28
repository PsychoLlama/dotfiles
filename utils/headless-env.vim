" Construct the runtime environment.
let s:dotfiles_dir = systemlist('dotfiles dir')[0]
let s:editor_env = s:dotfiles_dir . '/editor'

call execute('set runtimepath+=' . s:editor_env)

" Execute the source file.
func s:ExecuteSourceFile() abort
  let l:sourcefile = fnameescape(g:__sourcefile)
  unlet g:__sourcefile

  execute 'source ' . l:sourcefile
endfunc

let s:output = stacktrace#Capture(function('s:ExecuteSourceFile'))

" Determine exit code.
if s:output is g:stacktrace#Exception
  255 cquit
endif
