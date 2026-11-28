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
        nixpkgs.follows = "nixpkgs-unstable";
        systems.follows = "systems";
      };
    };

    navitron-nvim = {
      url = "github:PsychoLlama/navitron.nvim";
      inputs = {
        nixpkgs.follows = "nixpkgs-unstable";
        systems.follows = "systems";
      };
    };

    deja-view-nvim = {
      url = "github:PsychoLlama/deja-view.nvim";
      inputs = {
        nixpkgs.follows = "nixpkgs-unstable";
        systems.follows = "systems";
      };
    };

    gutenberg-nvim = {
      url = "github:PsychoLlama/gutenberg.nvim";
      inputs = {
        nixpkgs.follows = "nixpkgs-unstable";
        systems.follows = "systems";
      };
    };

    teleport-vim = {
      url = "github:PsychoLlama/teleport.vim";
      inputs = {
        nixpkgs.follows = "nixpkgs-unstable";
        systems.follows = "systems";
      };
    };

    rhizome = {
      url = "github:PsychoLlama/rhizome.nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        systems.follows = "systems";
      };
    };

    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs:

    let
      inherit (inputs) self;
      inherit (inputs.nixpkgs) lib;

      rhizome = inputs.rhizome.lib;
      units = rhizome.load ./units { inherit inputs lib; };
      inherit (units) editor;

      # One tree per machine in `units/hosts`.
      hosts = lib.mapAttrs (_: host: rhizome.eval { name = "dotfiles"; } host.unit) units.hosts;

      # What the graph prints for a box or an edge: the name without what is
      # the same everywhere. `platform.host.instance[nas-001]` is
      # `host[nas-001]`, and `services.ntfy.unit` is `services.ntfy`.
      display =
        { address, label }:
        let
          parts =
            lib.remove "platform" address.path
            ++ lib.optional (
              !lib.elem address.name [
                "unit"
                "instance"
              ]
            ) address.name;

          name = lib.concatStringsSep "." parts;
        in
        if label == null then name else "${name}[${label}]";

      # The editor's units, evaluated with no machine around them.
      standalone = rhizome.eval {
        name = "dotfiles";
        scopes = [
          units.platform.host.scope
          units.platform.user.scope
        ];
      } editor.profiles.full.unit;

      loadPkgs =
        system:
        import inputs.nixpkgs {
          inherit system;
          overlays = lib.attrValues self.overlays;
        };

      packages = lib.genAttrs (import inputs.systems) loadPkgs;
      eachSystem = lib.flip lib.mapAttrs packages;
    in

    {
      overlays = import ./pkgs/overlays.nix inputs;

      nixosConfigurations = lib.mapAttrs (
        _: host:
        lib.nixosSystem {
          modules = lib.concatMap (machine: machine.modules) (host.collect units.platform.nixos.machine);
        }
      ) hosts;

      # Who writes what to whom, as Graphviz. See `just graph`.
      graphs = lib.mapAttrs (_: host: rhizome.graph.dot { inherit display; } host.routes) hosts;

      packages = eachSystem (
        _: pkgs: {
          # `nix shell dotfiles#editor`
          editor = editor.lib.build pkgs {
            neovim = standalone.collect editor.neovim;
            plugins = standalone.collect editor.plugin;
            servers = standalone.collect editor.server;
            config = standalone.collect editor.config;
            tools = standalone.collect editor.tool;
          };

          inherit (pkgs.custom) chrome-devtools-mcp claude-code-bin codex-bin;
        }
      );

      devShells = eachSystem (
        system: pkgs: {
          default = pkgs.mkShell {
            packages = [
              inputs.agenix.packages.${system}.default
              pkgs.unstable.just
              pkgs.unstable.lua-language-server
              pkgs.unstable.luajitPackages.luacheck
              pkgs.unstable.luajitPackages.vusted
              pkgs.nh
              pkgs.unstable.nix-update
              pkgs.unstable.nixfmt
              pkgs.unstable.prettier
              pkgs.unstable.stylua
              pkgs.unstable.treefmt
            ];
          };
        }
      );

      # Scaffolds for other projects: `nix flake init -t dotfiles#rust`.
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
    };
}
