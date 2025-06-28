{
  description = "NixOS modules supporting my development environments";

  inputs = {
    nixos-hardware.url = "github:nixos/nixos-hardware";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    systems.url = "github:nix-systems/default";

    home-manager = {
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-darwin = {
      url = "github:LnL7/nix-darwin/nix-darwin-25.05";
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

    tree-sitter-remix.url = "github:PsychoLlama/tree-sitter-remix";
  };

  outputs =
    flake-inputs@{
      self,

      nixos-hardware,
      nixpkgs,
      nixpkgs-unstable,
      tree-sitter-remix,
      home-manager,
      systems,
      ...
    }:
    let
      lib = import ./lib flake-inputs;

      # Packages with unfree licenses. To be replaced with libre alternatives.
      evilPackages = [
        "claude-code"
        "copilot-language-server"
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

          config = {
            allowUnfreePredicate = pkg: lib.elem (lib.getName pkg) evilPackages;
          };
        };

      # { system -> pkgs }
      pkgsBySystem = lib.genAttrs (import systems) importPkgs;

      # (system: pkgs: a) -> { system -> a }
      eachSystem = lib.flip lib.mapAttrs pkgsBySystem;
    in

    {
      nixpkgs = pkgsBySystem;
      lib = lib.dotfiles // {
        inherit eachSystem;
      };

      nixosModules = {
        editor-platform = ./platforms/editor/modules;
        editor-configs = ./platforms/editor/modules/psychollama;

        home-manager-platform = ./platforms/home-manager/modules;
        home-manager-configs = ./platforms/home-manager/modules/psychollama;

        nixos-platform = ./platforms/nixos/modules;
        nixos-configs = ./platforms/nixos/modules/psychollama;
      };

      overlays = {
        latest-packages = import ./lib/overlays/latest-packages.nix flake-inputs;
        vim-plugins = import ./lib/overlays/vim-plugins.nix flake-inputs;
      };

      nixosConfigurations = lib.dotfiles.hosts.nixos {
        ava = {
          pkgs = pkgsBySystem.x86_64-linux;
          modules = [
            nixos-hardware.nixosModules.lenovo-thinkpad-p1-gen3
            nixpkgs.nixosModules.notDetected
            ./hosts/ava
          ];
        };
      };

      homeConfigurations = lib.dotfiles.hosts.home-manager {
        overlord = {
          pkgs = pkgsBySystem.x86_64-linux;
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

        lua = {
          description = "Flake environment for building Lua/Neovim projects";
          path = ./templates/lua;
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

          docs-website =
            pkgs.runCommand "generate-docs-website"
              {
                buildInputs = [ pkgs.mdbook ];
              }
              ''
                cp --no-preserve=mode --recursive "${self.outPath}/docs" docs/
                cp "${nixos-configs-doc}/options.md" docs/src/nixos.md
                cp "${editor-configs-doc}/options.md" docs/src/editor.md

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
              pkgs.luaPackages.luacheck
              pkgs.stylua
            ];
          };
        }
      );
    };
}
