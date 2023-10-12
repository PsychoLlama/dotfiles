{
  description = "NixOS modules supporting my development environment";

  inputs = {
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    hardware.url = "github:nixos/nixos-hardware";

    home-manager = {
      url = "github:nix-community/home-manager/release-23.05";
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

    unison-vim = {
      url = "github:unisonweb/unison";
      flake = false;
    };
  };

  outputs = inputs:
    let
      dlib = import ./lib inputs; # (d)otfiles lib
      inherit (inputs.nixpkgs-unstable) lib;
      inherit (inputs) nixpkgs-unstable;

      # The list of systems supported by nixpkgs and hydra.
      defaultSystems =
        [ "aarch64-linux" "aarch64-darwin" "x86_64-darwin" "x86_64-linux" ];

      pkgsFor = system:
        import inputs.nixpkgs-unstable {
          inherit system;
          overlays = [ inputs.self.overlays.vim-plugins ];
        };

      eachSystem = fn:
        inputs.nixpkgs-unstable.lib.pipe defaultSystems [
          (map (system: lib.nameValuePair system (pkgsFor system)))
          lib.listToAttrs
          (lib.mapAttrs fn)
        ];

    in {
      lib = dlib;

      nixosModules = {
        darwin = ./modules/darwin;
        editor = ./modules/editor;
        home-manager = ./modules/home-manager;
        nixos = ./modules/nixos;
      };

      overlays = {
        vim-plugins = import ./overlays/vim-plugins.nix inputs;
        latest-packages =
          import ./overlays/latest-packages.nix nixpkgs-unstable;
      };

      nixosConfigurations = {
        ava = dlib.defineHost.nixosSystem "x86_64-linux" ./hosts/ava;
      };

      darwinConfigurations = {
        marvin = dlib.defineHost.darwinSystem "x86_64-darwin" ./hosts/marvin;
      };

      homeConfigurations = {
        overlord =
          dlib.defineHost.homeManagerConfiguration "x86_64-linux" ./hosts/tars;
      };

      templates = {
        js = {
          description = "Create a JavaScript development environment";
          path = ./templates/js;
        };

        rust = {
          description = "Create a Rust development environment";
          path = ./templates/rust;
        };
      };

      packages = eachSystem (system: pkgs: {
        editor = dlib.buildEditor {
          inherit system;
          config.presets.base.enable = true;
        };
      });

      devShell = eachSystem (system: pkgs:
        pkgs.mkShell {
          buildInputs = [
            (dlib.buildEditor {
              inherit system;

              config = {
                presets.base.enable = true;
                plugins.personal-vim-config.enable = false;

                # Link to the mutable vim config so it can be edited without
                # a rebuild. Use `nix shell '.#editor'` to build the final.
                extraConfig = lib.mkBefore ''
                  let s:repo = systemlist('git rev-parse --show-toplevel')[0]
                  exe 'set rtp^=' . s:repo . '/config/editor'
                '';
              };
            })
          ];
        });
    };
}
