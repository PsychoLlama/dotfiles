{ inputs, ... }:

let
  inherit (inputs) import-tree;
in

{
  imports = [ inputs.flake-parts.flakeModules.modules ];

  # Every `.nix` file in these trees is a module. Helpers, data, and libraries
  # opt out with an `_` prefix, which import-tree ignores by default.
  #
  # `nixos` and `generic` have no platform extensions today. Add the output
  # alongside a `platform/` directory when they do.
  flake.modules = {
    generic.default = { };

    # Seeded so the outputs exist when nothing contributes to them.
    editor.default = { };
    homeManager.default = { };
    nixos.default = { };
  };
}
