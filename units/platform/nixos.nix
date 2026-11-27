{
  rhizome,
  units,
  self,
  define,
  ...
}:

let
  t = rhizome.types;
  inherit (units.platform) host;
in

{
  # A NixOS module for the host.
  module = define.fact (
    { many, as, ... }: {
      scope = host.scope;
      type = t.any;
      holds = many as.entries;
    }
  );

  # A machine to build, with every module written for it.
  machine = define.fact (
    { many, as, ... }: {
      holds = many as.list;

      type = {
        name = t.str;
        system = t.str;
        modules = t.list t.any;
      };
    }
  );

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

  unit = define.unit {
    uses = {
      inherit (host) name system;
      entries = self.module;
    };

    provides =
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
