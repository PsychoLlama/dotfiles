{ config, ... }:

# The editor evaluates in its own isolated module system, so it cannot read the
# trust list from a platform above it. Closing over the flake option sidesteps
# that entirely: `env.trusted` is fixed at flake evaluation, wherever the editor
# is later built. `~` is expanded at runtime by the env framework.

let
  inherit (config) trusted-directories;
in

{
  imports = [ ../system/trusted-directories.nix ];

  flake.modules.editor.default = {
    config.env.trusted = trusted-directories;
  };
}
