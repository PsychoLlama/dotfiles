use std "path add"
use std formats *
use std/util repeat
use std/iter

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

# Global `.env` for API keys. Mostly for generative models.
do --ignore-errors { open ~/.env.yml } | default {} | load-env
