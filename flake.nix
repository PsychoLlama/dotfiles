{
  description = "NixOS modules supporting my development environment";

  inputs = {
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05";
    hardware.url = "github:nixos/nixos-hardware";

    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # TODO: Put these vim plugins in nixpkgs.
    alternaut-vim = {
      url = "github:PsychoLlama/alternaut.vim";
      flake = false;
    };

    further-vim = {
      url = "github:PsychoLlama/further.vim";
      flake = false;
    };

    navitron-nvim = {
      url = "github:PsychoLlama/navitron.vim";
      flake = false;
    };

    teleport-vim = {
      url = "github:PsychoLlama/teleport.vim";
      flake = false;
    };

    unison-vim = {
      url = "github:unisonweb/unison";
      flake = false;
    };
  };

  outputs = inputs:
    let lib = import ./lib inputs;

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
