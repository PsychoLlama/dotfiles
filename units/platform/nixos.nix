{ rhizome, units, ... }:

let
  t = rhizome.types;
  inherit (units.platform) level;
in

rec {
  # A NixOS module for the host.
  module = rhizome.channel "nixos.module" {
    level = level.host;
    type.input = t.any;
    merge = rhizome.merge.entries;
  };

  # Everything a host hands to `nixosSystem`.
  modules = rhizome.channel "nixos.modules" {
    type.input = t.any;
    merge = rhizome.merge.list;
  };

  # Errors name the unit that wrote the module.
  locate =
    { from, value }:
    {
      _file = from;
      imports = [ value ];
    };

  host = rhizome.scope "host" {
    level = level.host;

    members = [
      (rhizome.component "nixos" {
        uses.entries = module;
        emits = [ modules ];
        config = { entries }: map (entry: modules (locate entry)) entries;
      })
    ];
  };
}
