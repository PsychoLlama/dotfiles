command! SN call stacktrace#Capture(function('editor#commands#SyntaxName'))

command! -nargs=? Permissions call stacktrace#Capture(
      \   function('editor#commands#Permissions', [<f-args>])
      \ )

command! -range -bang Author call stacktrace#Capture(
      \   function('editor#commands#Author', [<line1>, <line2>, <bang>0])
      \ )

command! -nargs=1 Readme call stacktrace#Capture(
      \   function('editor#commands#Readme', [<f-args>])
      \ )

command! Revert call stacktrace#Capture(
      \   function('editor#commands#Revert')
      \ )

command! Diff call stacktrace#Capture(
      \   function('editor#commands#Diff')
      \ )

command! Node call stacktrace#Capture(
      \   function('editor#commands#Node')
      \ )

command! Test call stacktrace#Capture(
      \   function('editor#commands#Test')
      \ )

command! -nargs=* Search call stacktrace#Capture(
      \   function('editor#commands#Search', [<f-args>])
      \ )
