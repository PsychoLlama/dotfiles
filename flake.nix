{
  description = "NixOS modules supporting my development environments";

  inputs = {
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    systems.url = "github:nix-systems/default";

    nixos-hardware = {
      url = "github:nixos/nixos-hardware";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    alternaut-nvim = {
      url = "github:PsychoLlama/alternaut.nvim";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        systems.follows = "systems";
      };
    };

    navitron-nvim = {
      url = "github:PsychoLlama/navitron.nvim";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        systems.follows = "systems";
      };
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
      inputs = {
        home-manager.follows = "home-manager";
        nixpkgs.follows = "nixpkgs";
        systems.follows = "systems";
      };
    };
  };

  outputs =
    flake-inputs@{
      self,

      agenix,
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
      lib = lib.dotfiles // {
        inherit eachSystem;
      };

      nixosModules = {
        universal-platform = {
          imports = lib.dotfiles.discoverNixFiles {
            directory = ./platforms/universal/modules;
          };
        };

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
        packages = import ./lib/overlays/packages.nix flake-inputs;
        vim-plugins = import ./lib/overlays/vim-plugins.nix flake-inputs;
      };

      nixosConfigurations = lib.dotfiles.hosts.nixos {
        ava = [
          nixos-hardware.nixosModules.lenovo-thinkpad-p1-gen3
          nixpkgs.nixosModules.notDetected
          ./hosts/ava
        ];
      };

      templates = {
        project = {
          description = "Flake environment with no assumptions";
          path = ./templates/project;
        };

        typescript = {
          description = "Flake environment for building TypeScript projects";
          path = ./templates/typescript;
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
        system: pkgs: {
          editor = lib.dotfiles.buildEditor {
            inherit pkgs;
            modules = [
              self.nixosModules.editor-configs
              { psychollama.profiles.full.enable = true; }
            ];
          };

          inherit (pkgs.custom) chrome-devtools-mcp claude-code-bin codex-bin;
        }
      );

      devShells = eachSystem (
        system: pkgs: {
          default = pkgs.mkShell {
            packages = [
              agenix.packages.${system}.default
              pkgs.just
              pkgs.lua-language-server
              pkgs.luajitPackages.luacheck
              pkgs.luajitPackages.vusted
              pkgs.nh
              pkgs.nix-update
              pkgs.nixfmt
              pkgs.prettier
              pkgs.stylua
              pkgs.treefmt
            ];
          };
        }
      );
    };
}
