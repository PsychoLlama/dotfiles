<div align="center">
  <h1>clippy.nvim</h1>
  <p>A fancy clipboard manager.</p>
</div>

## Purpose
Neovim's clipboard provider supports exactly one backend at a time, but I frequently find myself needing to switch clipboard backends. Clippy proxies the native integration allowing you to switch clipboards on the fly.

Specifically I like to use the tmux clipboard when I'm in a session, but occasionally I'll need to copy something to the system clipboard. Clippy allows me to switch to the Mac clipboard, yank some text, then swap back to tmux without damaging my workflow zen.

## Usage
```viml
" Copy two lines in linewise mode (:help setreg())
call clippy#copy(['first line', 'second line'], 'V')
echo clippy#paste() " -> [['first line', 'second line'], 'V']

" Swap between host and virtual clipboard backends (mac and tmux).
nmap <leader>a <Plug>(clippy-toggle-clipboard-mode)
```
