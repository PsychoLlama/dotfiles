command! -range -bang Author call stacktrace#capture(
\   function('accuse#command#', [<line1>, <line2>, <bang>0])
\ )
