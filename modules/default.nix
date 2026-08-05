{ inputs, ... }:

# Only flake-module trees belong here. The `<class>/` directories hold nixos,
# home-manager, and editor modules, imported via `flake.modules.*` instead.

{
  imports = [
    (inputs.import-tree [
      ./den
      ./dotfiles
      ./flake
      ./hosts
    ])
  ];
}
