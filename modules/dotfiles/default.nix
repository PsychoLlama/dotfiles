{ inputs, ... }:

{
  # TODO: Move overlays to the flake.
  config.nixpkgs.overlays = [
    inputs.self.overlays.latest-packages
    inputs.self.overlays.vim-plugins
    inputs.tree-sitter-remix.overlays.custom-grammars
  ];
}
