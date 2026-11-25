{
  rhizome,
  units,
  inputs,
  ...
}:

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

  # A machine. Starts its host scope and hands what the scope wrote to
  # `nixosSystem`.
  host = rhizome.scope "host" (input: {
    level = level.host;
    type.input = {
      name = t.str;
      system = t.str;
    };
    label = input.name;

    members = [
      (module {
        networking.hostName = input.name;
        nixpkgs.hostPlatform = input.system;

        # Surfaces the source commit in `nixos-version --json`.
        system.configurationRevision = inputs.self.rev or inputs.self.dirtyRev or null;
      })

      (rhizome.component "nixos" {
        uses.entries = module;
        emits = [ modules ];
        config = { entries }: map (entry: modules (locate entry)) entries;
      })
    ];
  });
}
