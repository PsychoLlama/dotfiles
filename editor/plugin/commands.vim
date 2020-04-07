command! SN call stacktrace#Capture(function('editor#commands#SyntaxName'))

command! -nargs=? Permissions call stacktrace#Capture(
      \   function('editor#commands#Permissions', [<f-args>])
      \ )

command! -range -bang Author call stacktrace#Capture(
      \   function('editor#commands#Author', [<line1>, <line2>, <bang>0])
      \ )

command! Node call stacktrace#Capture(
      \   function('editor#commands#Node')
      \ )

command! -nargs=* Search call stacktrace#Capture(
      \   function('editor#commands#Search', [<f-args>])
      \ )
