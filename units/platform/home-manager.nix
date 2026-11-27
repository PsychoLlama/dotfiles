# The way out of Home Manager is through it. A unit writes a Home Manager
# module here until it owns its files itself.
{
  rhizome,
  units,
  inputs,
  self,
  module,
  ...
}:

let
  t = rhizome.types;
  inherit (units.platform) nixos user;
in

{
  # A Home Manager module for the user.
  module = module.channel {
    level = user.level;
    type = t.any;
    merge = rhizome.merge.entries;
  };

  /**
    Installs a package from `pkgs.unstable`.

    # Type

    ```
    package :: String -> Emission
    ```
  */
  lib.package =
    name:
    self.module (
      { pkgs, ... }:
      {
        home.packages = [ pkgs.unstable.${name} ];
      }
    );

  /**
    Installs a program from `pkgs.unstable` through its Home Manager module.

    # Type

    ```
    program :: String -> Emission
    ```
  */
  lib.program =
    name:
    self.module (
      { lib, pkgs, ... }:
      {
        programs.${name} = {
          enable = lib.mkDefault true;
          package = lib.mkDefault pkgs.unstable.${name};
        };
      }
    );

  # Hands what the user's scope wrote to their account.
  unit = module.component {
    uses = {
      inherit (user) identity;
      entries = self.module;
    };

    config =
      { identity, entries }:
      [
        (nixos.module (
          { lib, ... }:
          {
            imports = [ inputs.home-manager.nixosModules.home-manager ];

            home-manager = {
              useGlobalPkgs = lib.mkDefault true;
              useUserPackages = lib.mkDefault true;
              users.${identity.username}.imports = map nixos.lib.locate entries;
            };
          }
        ))
      ];
  };
}
