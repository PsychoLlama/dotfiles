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
    generic.configs = import-tree ../generic/psychollama;

    editor = {
      platform = import-tree ../editor/platform;
      configs = import-tree ../editor/psychollama;
    };

    homeManager = {
      platform = import-tree ../homeManager/platform;
      configs = import-tree ../homeManager/psychollama;
    };

    nixos.configs = import-tree ../nixos/psychollama;
  };
}
