{
  config,
  inputs,
  den,
  ...
}:

let
  inherit (config.flake) modules overlays;
  inherit (inputs)
    agenix
    nixpkgs-unstable
    self
    ;
in

# Applied to every entity. Den injects `den.default` as a schema include for
# hosts, users, and homes alike, deduplicating across scopes.

{
  den.default.includes = [
    den.batteries.hostname

    # Projects `homeManager` keys from the host's aspect tree onto users, so a
    # cross-class aspect can be included once on the host.
    den.batteries.host-aspects

    # TODO: Move into respective program aspects once they exist.
    (den.batteries.unfree [
      "claude-code" # symlinkJoin wrapper from home-manager
      "claude-code-bin"
      "spotify"
    ])
  ];

  # Base configuration every host inherits.
  den.default.nixos =
    { lib, pkgs, ... }:

    {
      imports = [
        agenix.nixosModules.default
        modules.nixos.configs
        modules.generic.configs
      ];

      # Surface this flake's git revision in `nixos-version --json` so the
      # running system can be traced back to the source commit.
      system.configurationRevision = self.rev or self.dirtyRev or null;

      nixpkgs = {
        overlays = [
          overlays.unstable-packages
          overlays.custom-packages
          overlays.custom-patches
          overlays.vim-plugins
        ];

        # Pin `<nixpkgs>` and `flake:nixpkgs` to match system packages.
        flake = {
          source = lib.mkForce nixpkgs-unstable; # Stable is dumb. Live a little.
          setNixPath = lib.mkForce true;
          setFlakeRegistry = true;
        };
      };

      nix = {
        package = pkgs.nixVersions.latest;

        registry = {
          dotfiles.flake = self;
          unstable.flake = nixpkgs-unstable;
        };

        settings = {
          experimental-features = "nix-command flakes";
          flake-registry = null; # Disable default listings.
        };
      };

      # Den imports the home-manager module itself; these are just preferences.
      home-manager = {
        useGlobalPkgs = lib.mkDefault true;
        useUserPackages = lib.mkDefault true;
      };
    };

  # Base configuration every user inherits.
  den.default.homeManager =
    { lib, osConfig, ... }:

    {
      imports = [
        agenix.homeManagerModules.default
        modules.homeManager.platform
        modules.homeManager.configs
        modules.generic.configs
      ];

      # Inherit trusted directories from host platform.
      psychollama.trusted-directories = lib.mkDefault osConfig.psychollama.trusted-directories;
    };
}
