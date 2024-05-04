{
  description = "NixOS modules supporting my development environment";

  inputs = {
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    hardware.url = "github:nixos/nixos-hardware";

    home-manager = {
      url = "github:nix-community/home-manager/release-23.11";
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

    deja-view-vim = {
      url = "github:PsychoLlama/deja-view.vim";
      flake = false;
    };

    teleport-vim = {
      url = "github:PsychoLlama/teleport.vim";
      flake = false;
    };

    tree-sitter-remix.url = "github:PsychoLlama/tree-sitter-remix";
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      nixpkgs-unstable,
      tree-sitter-remix,
      ...
    }:
    let
      lib = import ./lib inputs;

      # The list of systems supported by nixpkgs and hydra.
      defaultSystems = [
        "aarch64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
        "x86_64-linux"
      ];

      importPkgs =
        system:
        import nixpkgs {
          inherit system;
          overlays = [
            tree-sitter-remix.overlays.custom-grammars
            self.overlays.latest-packages
            self.overlays.vim-plugins
          ];
        };

      # { system -> pkgs }
      pkgsBySystem = lib.genAttrs defaultSystems importPkgs;

      # (system: pkgs: a) -> { system -> a }
      eachSystem = lib.flip lib.mapAttrs pkgsBySystem;
    in

    {
      lib = lib.dotfiles;

      nixosModules = {
        darwin = ./modules/darwin;
        editor = ./nix-neovim/modules;
        home-manager = ./home-manager/modules;
        nixos = ./nixos/modules;
      };

      overlays = {
        vim-plugins = import ./lib/overlays/vim-plugins.nix inputs;
        latest-packages = import ./lib/overlays/latest-packages.nix nixpkgs-unstable;
      };

      nixosConfigurations = {
        ava = lib.dotfiles.defineHost.nixosSystem "x86_64-linux" ./hosts/ava;
      };

      homeConfigurations = {
        overlord = lib.dotfiles.defineHost.homeManagerConfiguration "x86_64-linux" ./hosts/tars;
      };

      templates = {
        project = {
          description = "Flake environment with no assumptions";
          path = ./templates/project;
        };

        js = {
          description = "Flake environment for building JavaScript projects";
          path = ./templates/js;
        };

        rust = {
          description = "Flake environment for building Rust projects";
          path = ./templates/rust;
        };
      };

      packages = eachSystem (
        system: pkgs: {
          editor = lib.dotfiles.buildEditor {
            inherit pkgs;
            modules = [ { presets.base.enable = true; } ];
          };
        }
      );
    };
}
