command! -nargs=? Permissions call stacktrace#Capture(
      \   function('editor#commands#Permissions', [<f-args>])
      \ )

command! -range Author call stacktrace#Capture(
      \   function('editor#commands#Author', [<line1>, <line2>])
      \ )

command! -nargs=1 Readme call stacktrace#Capture(
      \   function('editor#commands#Readme', [<f-args>])
      \ )

command! OpenTestFile call stacktrace#Capture(
      \   function('editor#commands#OpenTestFile')
      \ )

command! -nargs=+ Z call stacktrace#Capture(
      \   function('editor#commands#Z', [<f-args>])
      \ )

command! Reset call stacktrace#Capture(
      \   function('editor#commands#Reset')
      \ )

command! Diff call stacktrace#Capture(
      \   function('editor#commands#Diff')
      \ )

command! Node call stacktrace#Capture(
      \   function('editor#commands#Node')
      \ )
