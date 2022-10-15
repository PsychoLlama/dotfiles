inputs:

let
  overlays = [ inputs.vim-plugin-nursery.overlay (import ./pkgs/default.nix) ];

  createPackageLoader = system: path:
    import path {
      inherit system;

      # Add custom software to instances of nixpkgs.
      overlays = overlays;
    };

in {
  # Injects dotfiles, flake inputs, and baseline NixOS configuration.
  defineHost = system: path:
    let
      use = createPackageLoader system;
      nixpkgs-unstable = use inputs.nixpkgs-unstable;
      pkgs = use inputs.nixpkgs;
      systems = {
        "x86_64-darwin" = inputs.darwin.lib.darwinSystem;
        "aarch64-darwin" = inputs.darwin.lib.darwinSystem;
      };

      mkSystem = systems.${system} or inputs.nixpkgs.lib.nixosSystem;

    in mkSystem rec {
      inherit system;

      # Add stable and unstable package channels.
      specialArgs = { inherit system inputs nixpkgs-unstable; };

      modules = [
        ({ lib, pkgs, ... }: {
          # Hostnames are set by the directory's name.
          networking.hostName = lib.mkDefault (baseNameOf path);

          # Add custom packages.
          nixpkgs.overlays = overlays;

          # This can be removed once nix flakes ship standard.
          nix = {
            package = pkgs.nixUnstable;
            extraOptions = ''
              experimental-features = nix-command flakes
            '';
          };
        })

        # Load the dotfiles framework.
        ./default.nix

        # Do machine-specific configuration.
        path
      ] ++ (inputs.nixpkgs.lib.optional pkgs.stdenv.hostPlatform.isDarwin
        ./modules/macos);
    };
}
