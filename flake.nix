{
  description = "NixOS modules supporting my development environment";

  inputs = {
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    hardware.url = "github:nixos/nixos-hardware";

    home-manager = {
      url = "github:nix-community/home-manager/release-22.11";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # TODO: Put these vim plugins in nixpkgs.
    alternaut-vim = {
      url = "github:PsychoLlama/alternaut.vim";
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
    let
      lib = import ./lib inputs;
      inherit (inputs) nixpkgs-unstable;

    in {
      inherit lib;

      nixosModule = inputs.self.nixosModules.nixos;

      nixosModules = {
        nixos = import ./modules/nixos;
        darwin = import ./modules/darwin;
        home-manager = ./modules/home-manager;
      };

      overlays = {
        vim-plugins = import ./overlays/vim-plugins.nix inputs;
        latest-packages =
          import ./overlays/latest-packages.nix nixpkgs-unstable;
      };

      nixosConfigurations = {
        ava = lib.defineHost.nixosSystem "x86_64-linux" ./hosts/ava;
      };

      darwinConfigurations = {
        marvin = lib.defineHost.darwinSystem "x86_64-darwin" ./hosts/marvin;
      };

      defaultTemplate = {
        description = "Creates a host built on the dotfiles framework";
        path = ./template;
      };
    };
}
