{ rhizome, units, ... }:

let
  t = rhizome.types;
  inherit (units.platform) host;
in

rhizome.unit "nixos" (nixos: {
  # A NixOS module for the host.
  module = rhizome.channel {
    level = host.level;
    type = t.any;
    merge = rhizome.merge.entries;
  };

  # A machine to build, with every module written for it.
  machine = rhizome.channel {
    merge = rhizome.merge.list;

    type = {
      name = t.str;
      system = t.str;
      modules = t.list t.any;
    };
  };

  lib = {
    /**
      A module as its writer emitted it, wrapped so errors name the unit.

      # Type

      ```
      locate :: { from :: String, value :: Module } -> Module
      ```
    */
    locate =
      { from, value }:
      {
        _file = from;
        imports = [ value ];
      };
  };

  unit = rhizome.component {
    uses = {
      inherit (host) name system;
      entries = nixos.module;
    };

    config =
      {
        name,
        system,
        entries,
      }:
      [
        (nixos.machine {
          inherit name system;
          modules = map nixos.lib.locate entries;
        })
      ];
  };
})
