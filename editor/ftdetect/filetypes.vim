" vint: -ProhibitAutocmdWithNoGroup
autocmd BufNewFile,BufRead .eslintrc,.babelrc setfiletype json
autocmd BufNewFile,BufRead *.flow setfiletype javascript.jsx
autocmd BufNewFile,BufRead *.tsx set filetype=typescript.tsx
autocmd BufNewFile,BufRead .flowconfig setfiletype toml
autocmd BufNewFile,BufRead .tmux.conf setfiletype tmux
autocmd BufNewFile,BufRead *.ts set filetype=typescript
autocmd TermOpen * setfiletype terminal
