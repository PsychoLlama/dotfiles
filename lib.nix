inputs:

let
  overlays = [
    inputs.vim-plugin-nursery.overlay
    (import ./pkgs/default.nix)
  ];

  createPackageLoader = system: path: import path {
    inherit system;

    # Add custom software to instances of nixpkgs.
    overlays = overlays;
  };

in {
  # Injects dotfiles, flake inputs, and baseline NixOS configuration.
  defineHost = system: path:
    let
      use = createPackageLoader system;
      unstable = use inputs.nixpkgs-unstable;
      pkgs = use inputs.nixpkgs;

    in inputs.nixpkgs.lib.nixosSystem rec {
      inherit system;

      # Add stable and unstable package channels.
      specialArgs = {
        inherit system inputs unstable;
      };

      modules = [
        ({ lib, ... }: {
          # Hostnames are set by the directory's name.
          networking.hostName = lib.mkDefault (baseNameOf path);

          # Add custom packages.
          nixpkgs.overlays = overlays;

          # Show the dotfiles revision in `nixos-version`.
          system.configurationRevision = inputs.self.rev or null;
        })

        # Load the dotfiles framework.
        ./default.nix

        # Enable and configure features from dotfiles.
        ./hosts/common.nix

        # Do machine-specific configuration.
        path
      ];
    };
}
