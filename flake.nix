{
  description = "NixOS modules supporting my development environment";

  inputs = {
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05";
    hardware.url = "github:nixos/nixos-hardware";

    vim-plugin-nursery = {
      url = "github:PsychoLlama/vim-plugin-nursery";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs:
    let lib = import ./lib.nix inputs;

    in {
      inherit lib;

      nixosModules.default = import ./default.nix;
      overlay = import ./pkgs;

      nixosConfigurations = {
        ava = lib.defineHost "x86_64-linux" ./hosts/ava;
      };

      darwinConfigurations = {
        marvin = lib.defineHost "x86_64-darwin" ./hosts/marvin;
      };

      defaultTemplate = {
        description = "Creates a host built on the dotfiles framework";
        path = ./template;
      };
    };
}
