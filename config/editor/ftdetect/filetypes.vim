" vint: -ProhibitAutocmdWithNoGroup
autocmd BufNewFile,BufRead .eslintrc,.babelrc setfiletype json
autocmd BufNewFile,BufRead *.tsx set filetype=typescript.tsx
autocmd BufNewFile,BufRead *.ts set filetype=typescript
autocmd BufNewFile,BufRead *.nomad setfiletype hcl
autocmd BufNewFile,BufRead *.nix setlocal commentstring=#%s
