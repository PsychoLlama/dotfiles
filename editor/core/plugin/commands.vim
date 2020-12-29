command! SN call stacktrace#capture(function('editor#commands#syntax_name'))

command! -nargs=? Permissions call stacktrace#capture(
      \   function('editor#commands#permissions', [<f-args>])
      \ )

command! Node call stacktrace#capture(
      \   function('editor#commands#node')
      \ )
