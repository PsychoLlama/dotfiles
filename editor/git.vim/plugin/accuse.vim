command! -range -bang Author call stacktrace#capture(
\   function('git#commands#author', [<line1>, <line2>, <bang>0])
\ )
