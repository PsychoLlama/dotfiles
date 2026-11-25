# The way out of Home Manager is through it. A unit writes a Home Manager
# module here until it owns its files itself.
{ rhizome, units, ... }:

let
  t = rhizome.types;
  inherit (units.platform) level nixos;
  inherit (units.platform.user) identity;
in

rec {
  module = rhizome.channel "home-manager.module" {
    level = level.user;
    type.input = t.any;
    merge = rhizome.merge.entries;
  };

  # A person on the machine. Starts their user scope, says who they are, and
  # hands what the scope wrote for Home Manager to their account.
  user = rhizome.scope "user" (input: {
    level = level.user;
    type.input = identity.type.input;
    label = input.username;

    members = [
      (identity input)

      (rhizome.component "home-manager" {
        uses.entries = module;

        config =
          { entries }:
          [ (nixos.module { home-manager.users.${input.username}.imports = map nixos.locate entries; }) ];
      })
    ];
  });

  # Installs a program from `pkgs.unstable` through its Home Manager module.
  program =
    name:
    module (
      { lib, pkgs, ... }:
      {
        programs.${name} = {
          enable = lib.mkDefault true;
          package = lib.mkDefault pkgs.unstable.${name};
        };
      }
    );
}
