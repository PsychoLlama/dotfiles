{ lib, ... }:

let
  /**
    A `programs.<name>` module for packages upstream doesn't cover, where enable
    and package are the only options worth having.

    # Inputs

    `pkgName`
    : Both the option name under `programs` and the nixpkgs attribute it
      defaults its package to.

    # Type

    ```
    program :: String -> Module
    ```
  */
  program =
    pkgName:

    # Named, because one file declaring 32 programs would otherwise report every
    # one of them as `default.nix` and leave an error pointing at the whole list.
    lib.setDefaultModuleLocation "${toString ./default.nix}#programs.${pkgName}" (
      { config, pkgs, ... }:

      let
        cfg = config.programs.${pkgName};
      in

      {
        options.programs.${pkgName} = {
          enable = lib.mkEnableOption "Whether to install ${pkgName}";
          package = lib.mkPackageOption pkgs pkgName { };
        };

        config.home.packages = lib.mkIf cfg.enable [ cfg.package ];
      }
    );
in

{
  # Programs earning nothing but an enable and a package. Anything with settings
  # of its own gets a file beside this one.
  flake.homeModules.platform.imports = map program [
    "acpi"
    "bemoji"
    "binutils"
    "brightnessctl"
    "dive"
    "dix"
    "dogdns"
    "doggo"
    "du-dust"
    "duf"
    "grim"
    "hexyl"
    "lsof"
    "miniserve"
    "nix-output-monitor"
    "onefetch"
    "pamixer"
    "parted"
    "playerctl"
    "pv"
    "python3"
    "rage"
    "signal-desktop"
    "slurp"
    "termshark"
    "tokei"
    "viddy"
    "wf-recorder"
    "whois"
    "wireplumber"
    "wl-clipboard"
    "wtype"
  ];
}
