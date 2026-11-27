{
  rhizome,
  units,
  self,
  module,
  ...
}:

let
  t = rhizome.types;
  inherit (units.platform) host;
in

{
  # A NixOS module for the host.
  module = module.channel "module" {
    level = host.level;
    type = t.any;
    merge = rhizome.merge.entries;
  };

  # A machine to build, with every module written for it.
  machine = module.channel "machine" {
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

  unit = module.component "unit" {
    uses = {
      inherit (host) name system;
      entries = self.module;
    };

    config =
      {
        name,
        system,
        entries,
      }:
      [
        (self.machine {
          inherit name system;
          modules = map self.lib.locate entries;
        })
      ];
  };
}
