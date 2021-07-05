inputs: rec {
  # Injects the dotfiles framework and any flake inputs.
  defineHost = path: inputs.nixpkgs-unstable.lib.nixosSystem rec {
    system = "x86_64-linux";

    specialArgs = {
      inherit system inputs;
    };

    modules = [
      # Load the dotfiles framework.
      ./default.nix

      # Make the networking hostname match the containing directory.
      { networking.hostName = baseNameOf path; }

      # Do machine-specific configuration.
      path
    ];
  };
}
