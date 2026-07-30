{ lib }:

# Rhizome: a module system layered over the nixpkgs one. A rhizome
# module declares options once and carries configuration for several
# platforms (nixos, home-manager, custom classes like the editor).
# Plugins are collections of such modules, shippable through flakes; a
# mount installs them into a host fixpoint, where enablement — not
# loading — is the cut.

let
  mkMount = import ./mk-mount.nix { inherit lib; };
in

{
  inherit mkMount;

  # Define a plugin from a directory of `*.mod.nix` files.
  plugin = import ./plugin.nix { inherit lib; };

  # Preassembled mounts per host platform: `mounts.nixos { <binding> =
  # <plugin>; }` returns a module for that platform's eval.
  mounts = {
    nixos = import ./mounts/nixos.nix { inherit lib mkMount; };
    home-manager = import ./mounts/home-manager.nix { inherit lib mkMount; };
  };

  # Unit tests (lib.runTests): empty list means success.
  #   nix eval .#lib.rhizome.tests --json
  tests = import ./tests { inherit lib; };
}
