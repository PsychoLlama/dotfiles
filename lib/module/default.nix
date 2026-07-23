{ lib }:

# A meta-module system: one module declares options once and carries
# configuration for several platforms (nixos, home-manager, custom
# classes like the editor). Plugins are collections of such modules,
# shippable through flakes; a "root guest" mounts them into a host
# root's own fixpoint, where enablement — not loading — is the cut.
#
# See sketches/6-root-guest.nix for the design notes.

let
  mkRoot = import ./mk-root.nix { inherit lib; };
in

{
  inherit mkRoot;

  # Define a plugin from a directory of `*.mod.nix` files.
  plugin = import ./plugin.nix { inherit lib; };

  # Preassembled guests per host platform: `roots.nixos { <binding> =
  # <plugin>; }` returns a module for that platform's eval.
  roots = {
    nixos = import ./roots/nixos.nix { inherit lib mkRoot; };
    home-manager = import ./roots/home-manager.nix { inherit lib mkRoot; };
  };

  # Unit tests (lib.runTests): empty list means success.
  #   nix eval .#lib.module.tests --json
  tests = import ./tests { inherit lib; };
}
