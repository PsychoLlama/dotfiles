# The way out of Home Manager is through it. A unit writes a Home Manager
# module here until it owns its files itself.
{ rhizome, units, ... }:

let
  t = rhizome.types;
  inherit (units.platform) level nixos;
in

rec {
  module = rhizome.channel "home-manager.module" {
    level = level.user;
    type.input = t.any;
    merge = rhizome.merge.entries;
  };

  user = rhizome.scope "user" (input: {
    level = level.user;
    type.input.name = t.str;
    label = input.name;

    members = [
      (rhizome.component "home-manager" {
        uses.entries = module;

        config =
          { entries }:
          [ (nixos.module { home-manager.users.${input.name}.imports = map nixos.locate entries; }) ];
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
