# A machine, and what is true of all of it.
{
  rhizome,
  units,
  inputs,
  ...
}:

let
  t = rhizome.types;
  inherit (units.platform) nixos;
in

rhizome.unit "host" (
  host:

  let
    fact =
      type:
      rhizome.channel {
        level = host.level;
        inherit type;
        merge = rhizome.merge.one;
        optional = true;
      };
  in

  {
    level = rhizome.level { within = rhizome.root; };

    name = fact t.str;
    system = fact t.str;

    scope = host.level.scope (cfg: {
      label = cfg.name;

      type = {
        name = host.name.type;
        system = host.system.type;
      };

      config = [
        (host.name cfg.name)
        (host.system cfg.system)

        (nixos.module {
          networking.hostName = cfg.name;
          nixpkgs.hostPlatform = cfg.system;

          # Surfaces the source commit in `nixos-version --json`.
          system.configurationRevision = inputs.self.rev or inputs.self.dirtyRev or null;
        })

        nixos.unit
      ];
    });
  }
)
