autocmd BufNewFile,BufRead .eslintrc,.babelrc setfiletype json
autocmd BufNewFile,BufRead *.nomad setfiletype hcl
autocmd BufNewFile,BufRead *.nix setlocal commentstring=#%s
autocmd BufNewFile,BufRead *.ncl setlocal filetype=nickel commentstring=#%s
autocmd BufNewFile,BufRead *.nu setlocal filetype=nu commentstring=#%s
autocmd BufNewFile,BufRead {*.go,go.mod} setlocal noexpandtab
autocmd BufNewFile,BufRead *.mdx setfiletype markdown.mdx
