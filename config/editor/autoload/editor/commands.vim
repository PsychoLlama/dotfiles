" :Nix
func! editor#commands#nix() abort
  new Nix Repl
  wincmd J
  resize 10
  setlocal nonumber signcolumn=no
  call termopen('nix repl -f flake:nixpkgs --offline')
  normal! A
endfunc


" :Perm +x
func! editor#commands#permissions(...) abort
  let l:file_path = expand('%:p')
  let l:file = fnameescape(l:file_path)

  if !filereadable(l:file_path)
    echo 'Wait, you sure this is a file?'
    return
  endif

  if len(a:000) == 0
    echo getfperm(l:file)
    return
  endif

  let l:output = system('chmod ' . a:1 . ' ' . l:file)

  if v:shell_error
    echoerr l:output
  endif
endfunc
