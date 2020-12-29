<div align="center">
  <h1>stacktrace.vim</h1>
  <p>A stacktrace parser for easier debugging.</p>
</div>

## Purpose
For any code that makes heavy use of script-scoped functions, lambdas, and funcrefs, the built-in vim stack trace is unbearable. This is a stacktrace parser and error handler that turns convoluted stack traces into easily understandable postmortems. Here's an example:

**Before**

```
Error detected while processing function editor#commands#author[13]..<SNR>201_print_line_details[1]..git#blame#[8]..<SNR>200_parse_blame_output[1]..<SNR>200_ge
t_user_name:
line    2:
E117: Unknown function: famemodify
E15: Invalid expression: famemodify(a:file, ':h')
Error detected while processing function editor#commands#author[13]..<SNR>201_print_line_details:
line    1:
E714: List required
```

**After**

```
Error: Vim(let):E117: Unknown function: famemodify
core/autoload/git/blame.vim line 128 in s:get_user_name (line 2)
  let l:containing_folder = famemodify(a:file, ':h')

core/autoload/git/blame.vim line 142 in s:parse_blame_output (line 1)
  let l:my_name = s:get_user_name(a:config.file)

core/autoload/git/blame.vim line 254 in git#blame# (line 8)
  let l:ownerships = s:parse_blame_output(l:output, a:blame)

/nix/store/l893d3yjg0bvzbimv0g00mrk3x5iqpx6-vimplugin-plugin-nursery-core-latest/share/vim-plugins/plugin-nursery-core-latest/autoload/editor/commands.vim line
 48 in s:print_line_details (line 1)
  let [l:details] = git#blame#({   'include_all_commits': a:all_commits,   'ranges': [[a:line, a:line]],   'file': expand('%:p'), })

/nix/store/l893d3yjg0bvzbimv0g00mrk3x5iqpx6-vimplugin-plugin-nursery-core-latest/share/vim-plugins/plugin-nursery-core-latest/autoload/editor/commands.vim line
 70 in editor#commands#author (line 13)
    return s:print_line_details(a:start, a:all_commits)

/nix/store/l893d3yjg0bvzbimv0g00mrk3x5iqpx6-vimplugin-plugin-nursery-core-latest/share/vim-plugins/plugin-nursery-core-latest/autoload/stacktrace.vim line 214
in stacktrace#capture (line 2)
    return a:capture_target()
```

Over time I've learned patterns that produce clearer stack traces, but this is still useful for my older code. I may delete this once the older code is gone. Plus it was really fun to write.
