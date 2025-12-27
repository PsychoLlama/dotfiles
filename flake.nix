{
  description = "NixOS modules supporting my development environments";

  inputs = {
    nixos-hardware.url = "github:nixos/nixos-hardware";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    systems.url = "github:nix-systems/default";

    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-darwin = {
      url = "github:nix-darwin/nix-darwin/nix-darwin-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    alternaut-nvim = {
      url = "github:PsychoLlama/alternaut.nvim";
      inputs.systems.follows = "systems";
    };

    navitron-nvim = {
      url = "github:PsychoLlama/navitron.nvim";
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

    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };

  };

  outputs =
    flake-inputs@{
      self,

      nixos-hardware,
      nixpkgs,
      nixpkgs-unstable,
      home-manager,
      systems,
      ...
    }:

    let
      lib = import ./lib flake-inputs;

      # { system -> pkgs }
      pkgsBySystem = lib.genAttrs (import systems) (
        system:
        import nixpkgs {
          inherit system;
          overlays = nixpkgs.lib.attrValues self.overlays;
        }
      );

      # (system: pkgs: a) -> { system -> a }
      eachSystem = lib.flip lib.mapAttrs pkgsBySystem;
    in

    {
      nixpkgs = pkgsBySystem;
      lib = lib.dotfiles // {
        inherit eachSystem;
      };

      nixosModules = {
        editor-platform = {
          imports = lib.dotfiles.discoverNixFiles {
            directory = ./platforms/editor/modules;
            exclude = [ ./platforms/editor/modules/psychollama ];
          };
        };

        editor-configs = {
          imports = lib.dotfiles.discoverNixFiles {
            directory = ./platforms/editor/modules/psychollama;
          };
        };

        home-manager-platform = {
          imports = lib.dotfiles.discoverNixFiles {
            directory = ./platforms/home-manager/modules;
            exclude = [ ./platforms/home-manager/modules/psychollama ];
          };
        };

        home-manager-configs = {
          imports = lib.dotfiles.discoverNixFiles {
            directory = ./platforms/home-manager/modules/psychollama;
          };
        };

        nixos-platform = {
          imports = lib.dotfiles.discoverNixFiles {
            directory = ./platforms/nixos/modules;
            exclude = [ ./platforms/nixos/modules/psychollama ];
          };
        };

        nixos-configs = {
          imports = lib.dotfiles.discoverNixFiles {
            directory = ./platforms/nixos/modules/psychollama;
          };
        };
      };

      overlays = {
        latest-packages = import ./lib/overlays/latest-packages.nix flake-inputs;
        vim-plugins = import ./lib/overlays/vim-plugins.nix flake-inputs;
      };

      nixosConfigurations = lib.dotfiles.hosts.nixos {
        ava = [
          nixos-hardware.nixosModules.lenovo-thinkpad-p1-gen3
          nixpkgs.nixosModules.notDetected
          ./hosts/ava
        ];
      };

      homeConfigurations = lib.dotfiles.hosts.home-manager {
        overlord = {
          system = "x86_64-linux";
          modules = [ ./hosts/tars ];
        };
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

        nvim = {
          description = "Flake environment for building Neovim plugins";
          path = ./templates/nvim;
        };
      };

      packages = eachSystem (
        system: pkgs: rec {
          nixos-configs-doc = pkgs.callPackage lib.dotfiles.generateMarkdownDocs {
            platform = "nixos";
            prefix = "psychollama.";
            modules = [
              home-manager.nixosModules.home-manager
              self.nixosModules.nixos-platform
              self.nixosModules.nixos-configs
            ];
          };

          editor-configs-doc = pkgs.callPackage lib.dotfiles.generateMarkdownDocs {
            prefix = "psychollama.";
            modules = [
              self.nixosModules.editor-platform
              self.nixosModules.editor-configs
            ];
          };

          home-manager-configs-doc = pkgs.callPackage lib.dotfiles.generateMarkdownDocs {
            platform = "home-manager";
            prefix = "psychollama.";
            modules = [
              self.nixosModules.home-manager-platform
              {
                imports = lib.dotfiles.discoverNixFiles {
                  directory = ./platforms/home-manager/modules/psychollama;
                  exclude = [ ./platforms/home-manager/modules/psychollama/presets/programs/editor.nix ];
                };

                # Stub for editor preset (requires `programs.editor` from hosts.nix).
                options.psychollama.presets.programs.editor.enable =
                  lib.mkEnableOption "Configure editor as the one true editor";
              }
            ];
          };

          docs-website =
            pkgs.runCommand "generate-docs-website"
              {
                buildInputs = [ pkgs.mdbook ];
              }
              ''
                cp --no-preserve=mode --recursive "${self.outPath}/docs" docs/
                cp "${nixos-configs-doc}/options.md" docs/src/nixos.md
                cp "${editor-configs-doc}/options.md" docs/src/editor.md
                cp "${home-manager-configs-doc}/options.md" docs/src/home-manager.md

                ls -lAh docs

                mdbook build docs/ --dest-dir "$out"
              '';

          editor = lib.dotfiles.buildEditor {
            inherit pkgs;
            modules = [
              self.nixosModules.editor-configs
              { psychollama.profiles.full.enable = true; }
            ];
          };
        }
      );

      devShells = eachSystem (
        system: pkgs: {
          default = pkgs.mkShell {
            packages = [
              pkgs.just
              pkgs.lua-language-server
              pkgs.luajitPackages.luacheck
              pkgs.luajitPackages.vusted
              pkgs.nixfmt-rfc-style
              pkgs.stylua
              pkgs.treefmt
            ];
          };
        }
      );
    };
}
