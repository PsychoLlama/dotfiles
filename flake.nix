{
  description = "NixOS modules supporting my development environment";

  inputs = {
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05";
    hardware.url = "github:nixos/nixos-hardware";

    home-manager = {
      url = "github:nix-community/home-manager/release-22.05";
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
    let
      lib = import ./lib inputs;
      inherit (inputs) nixpkgs-unstable;

    in {
      inherit lib;

      nixosModule = inputs.self.nixosModules.dotfiles;

      nixosModules = {
        dotfiles = import ./default.nix;
        home-manager = ./modules/home-manager;
      };

      overlays = {
        vim-plugins = import ./overlays/vim-plugins.nix inputs;
        latest-packages =
          import ./overlays/latest-packages.nix nixpkgs-unstable;
      };

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

      # EXPERIMENTAL: This exports preconfigured packages from the dotfiles
      # framework by building a faux NixOS system, enabling a few programs,
      # and extracting the generated packages from the module system.
      packages = with inputs.nixpkgs.lib;
        genAttrs systems.flakeExposed (system:
          let
            nixpkgs-unstable = inputs.nixpkgs-unstable.legacyPackages.${system};
            nixosSystem = inputs.nixpkgs.lib.nixosSystem {
              inherit system;

              specialArgs = { inherit system inputs nixpkgs-unstable; };

              modules = [
                inputs.self.nixosModules.dotfiles
                inputs.home-manager.nixosModule
                {
                  dotfiles = {
                    bleeding-edge = true;
                    user = {
                      manage = true;
                      account = "temp";
                      fullName = "Surprising Hackaround";
                    };
                  };

                  home-manager.users.temp = {
                    imports = [ inputs.self.nixosModules.home-manager ];
                    presets.neovim.enable = true;
                  };
                }
              ];
            };

          in {
            editor =
              nixosSystem.config.home-manager.users.temp.programs.neovim.finalPackage;
          });
    };
}
