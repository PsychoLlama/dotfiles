use std "path add"
use std formats *
use std/util repeat
use std/iter
use std/clip copy

# NixOS does not manage nushell and will not automatically add system paths to
# the RC. I have to add them manually.
#
# Adapted from `$PATH` variables set downstream of `/etc/bashrc`.
if (sys host | get name | $in != "Darwin") {
  # NixOS system binaries.
  path add /run/current-system/sw/bin
  path add /nix/var/nix/profiles/default/bin

  # Home Manager user binaries.
  path add $"/etc/profiles/per-user/($env.USER)/bin"

  # Directories supporting `nix-env` and `nix profile`.
  path add ~/.local/state/nix/profile/bin
  path add ~/.nix-profile/bin

  # Security wrappers, e.g. `sudo` and `mount`
  path add /run/wrappers/bin
}

$env.PROMPT_INDICATOR = { || "" }
$env.PROMPT_INDICATOR_VI_INSERT = { || "" }
$env.PROMPT_INDICATOR_VI_NORMAL = { || "" }
$env.PROMPT_MULTILINE_INDICATOR = { || "" }

# Auto-navigate new tmux sessions to the zoxide-resolved directory matching
# the session name. Only the first shell in a fresh session sees one pane.
if ($env.TMUX? | is-not-empty) {
  let pane_count = tmux list-panes -s -F '#{pane_id}' | lines | length
  let session = tmux display -p '#{session_name}'

  # Skip tmux's default numeric session names.
  if $pane_count == 1 and not ($session =~ '^\d+$') {
    let target = do --ignore-errors { zoxide query $session | str trim }
    if ($target | is-not-empty) {
      cd $target
    }
  }
}
