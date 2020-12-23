command! SN call stacktrace#capture(function('editor#commands#syntax_name'))

command! -nargs=? Permissions call stacktrace#capture(
      \   function('editor#commands#permissions', [<f-args>])
      \ )

command! -range -bang Author call stacktrace#capture(
      \   function('editor#commands#author', [<line1>, <line2>, <bang>0])
      \ )

command! Node call stacktrace#capture(
      \   function('editor#commands#node')
      \ )

command! -nargs=* Search call stacktrace#capture(
      \   function('editor#commands#search', [<f-args>])
      \ )
