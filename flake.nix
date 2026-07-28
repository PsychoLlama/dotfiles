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

    deja-view-nvim = {
      url = "github:PsychoLlama/deja-view.nvim";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        systems.follows = "systems";
      };
    };

    gutenberg-nvim = {
      url = "github:PsychoLlama/gutenberg.nvim";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        systems.follows = "systems";
      };
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

      # This flake's own instances, mounted by `nixosConfigurations` and the
      # portable editor below. Consumers instantiate their own the same way:
      # `inputs.dotfiles.plugins.dotfiles { }`.
      dotfiles = self.plugins.dotfiles { };
      hosts = self.plugins.hosts { inherit dotfiles; };
    in

    {
      lib = lib.dotfiles // {
        inherit eachSystem;
      };

      # Meta-modules: one module per program carrying payloads for every
      # platform it touches. These are plugin *definitions* — apply one to an
      # input set to instantiate, then mount with `lib.module.roots.<class>`.
      plugins = {
        dotfiles = lib.dotfiles.module.plugin {
          src = ./modules/dotfiles;
          classes.editor = "editor";
        };

        # Machine-specific settings. Split from the dotfiles so a consumer can
        # mount the opinions without inheriting my hardware, and so a host says
        # which plugin it is configuring rather than sitting inside it.
        hosts = lib.dotfiles.module.plugin {
          src = ./modules/hosts;
          inputs.dotfiles = throw "hosts: input `dotfiles` is required.";
        };
      };

      nixosModules = {
        editor-platform = {
          imports = lib.dotfiles.discoverNixFiles {
            directory = ./platforms/editor/modules;
          };
        };

        home-manager-platform = {
          imports = lib.dotfiles.discoverNixFiles {
            directory = ./platforms/home-manager/modules;
          };
        };
      };

      overlays = {
        latest-packages = import ./lib/overlays/latest-packages.nix flake-inputs;
        packages = import ./lib/overlays/packages.nix flake-inputs;
        vim-plugins = import ./lib/overlays/vim-plugins.nix flake-inputs;
      };

      nixosConfigurations = lib.dotfiles.hosts.nixos { inherit dotfiles hosts; } {
        ava = [
          # Flake inputs can only be imported at the assembly site; a
          # meta-module has no `imports`.
          nixos-hardware.nixosModules.lenovo-thinkpad-p1-gen3
          nixpkgs.nixosModules.notDetected

          { "${hosts}".ava.enable = true; }
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
            plugin = dotfiles;
            modules = [ { "${dotfiles}".profiles.editor.enable = true; } ];
          };

          inherit (pkgs.custom) chrome-devtools-mcp claude-code-bin codex-bin;
        }
      );

      devShells = eachSystem (
        system: pkgs: {
          default = pkgs.mkShell {
            packages = [
              agenix.packages.${system}.default
              pkgs.unstable.just
              pkgs.unstable.lua-language-server
              pkgs.unstable.luajitPackages.luacheck
              pkgs.unstable.luajitPackages.vusted
              pkgs.unstable.nh
              pkgs.unstable.nix-update
              pkgs.unstable.nixfmt
              pkgs.unstable.prettier
              pkgs.unstable.stylua
              pkgs.unstable.treefmt
            ];
          };
        }
      );
    };
}
