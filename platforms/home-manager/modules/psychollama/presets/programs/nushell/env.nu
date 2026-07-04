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

# Configure new tmux sessions from their first shell. Only the first shell in a
# fresh session sees a single pane.
if ($env.TMUX? | is-not-empty) {
  let pane_count = tmux list-panes -s -F '#{pane_id}' | lines | length
  let session = tmux display -p '#{session_name}'

  if $pane_count == 1 {
    # Name the first window consistently. This also disables tmux's automatic
    # renaming for the window.
    tmux rename-window main

    # Auto-navigate to the zoxide-resolved directory matching the session name,
    # skipping tmux's default numeric session names. `complete` captures
    # zoxide's "no match found" warning instead of leaking it to the terminal.
    if not ($session =~ '^\d+$') {
      let match = zoxide query $session | complete
      if $match.exit_code == 0 {
        cd ($match.stdout | str trim)
      }
    }
  }
}
