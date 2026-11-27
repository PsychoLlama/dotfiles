# A machine, and what is true of all of it.
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
  inherit (units.platform) nixos;

  fact =
    name: type:
    module.channel name {
      level = self.level;
      inherit type;
      merge = rhizome.merge.one;
      optional = true;
    };
in

{
  level = module.level "level" { within = rhizome.root; };

  name = fact "name" t.str;
  system = fact "system" t.str;

  scope = self.level.scope module "scope" (cfg: {
    label = cfg.name;

    input = {
      name = self.name.type;
      system = self.system.type;
    };

    config = [
      (self.name cfg.name)
      (self.system cfg.system)

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
