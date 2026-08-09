{ inputs, ... }:

{
  imports = [ inputs.flake-parts.flakeModules.modules ];

  flake.modules = {
    # Seeded so the outputs exist when nothing contributes to them.
    editor.default = { };
    homeManager.default = { };
    nixos.default = { };
  };
}
