inputs: system: path:

# Injects dotfiles, flake inputs, and baseline NixOS configuration.

let
  use = flake: flake.legacyPackages.${system};

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

      # Include custom NixOS modules in the generated man page.
      documentation.nixos.extraModuleSources =
        [ "${inputs.home-manager}/nixos" ];

      # This can be removed once nix flakes ship standard.
      nix = {
        package = pkgs.nixUnstable;
        extraOptions = ''
          experimental-features = nix-command flakes
        '';
      };
    })

    # Add home-manager for easier cross-platform support.
    inputs.home-manager.nixosModules.home-manager

    # Load the dotfiles framework.
    inputs.self.nixosModules.dotfiles

    # Do machine-specific configuration.
    path
  ] ++ (inputs.nixpkgs.lib.optional pkgs.stdenv.hostPlatform.isDarwin
    ../modules/macos);
}
