inputs: rec {
  # Merges `inputs` with other module arguments.
  #
  # path: path to the nixos module.
  # nixos: parameters passed to the nixos module.
  defineModule = path: nixos: import path (nixos // {
    inherit inputs;
  });

  # Injects the dotfiles framework and any flake inputs.
  defineHost = path: nixos: {
    imports = [
      # Load the dotfiles framework.
      (defineModule ./default.nix)

      # Make the networking hostname match the containing directory.
      { networking.hostname = baseNameOf path; }

      # Do machine-specific configuration.
      (defineModule path)
    ];
  };
}
