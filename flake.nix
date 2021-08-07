{
  description = "NixOS modules supporting my development environment";

  inputs.nixpkgs-unstable.url = "nixpkgs/nixpkgs-unstable";
  inputs.nixpkgs.url = "nixpkgs/nixos-21.05";

  inputs.hardware.url = "github:nixos/nixos-hardware/master";
  inputs.vim-plugin-nursery.url = "github:PsychoLlama/vim-plugin-nursery/main";
  inputs.vim-plugin-nursery.inputs.nixpkgs.follows = "nixpkgs";

  outputs = inputs:
    let lib = import ./lib.nix inputs;

    in {
      nixosModule = import ./default.nix;
      overlay = import ./pkgs;

      nixosConfigurations = {
        ava = lib.defineHost "x86_64-linux" ./hosts/ava;
      };

      defaultTemplate = {
        description = "Creates a host built on the dotfiles framework";
        path = ./template;
      };
    };
}
