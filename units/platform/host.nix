# A machine, and what is true of all of it.
{
  rhizome,
  units,
  inputs,
  self,
  define,
  ...
}:

let
  t = rhizome.types;
  inherit (units.platform) nixos;

  fact =
    type:
    define.fact (
      { one, ... }: {
        scope = self.scope;
        inherit type;
        holds = one;
        allow-unread = true;
      }
    );
in

{
  scope = define.scope { within = rhizome.root; };

  name = fact t.str;
  system = fact t.str;

  instance = self.scope.instance (input: {
    label = input.name;

    type = {
      name = self.name.type;
      system = self.system.type;
    };

    includes = [
      (self.name input.name)
      (self.system input.system)

      (nixos.module {
        networking.hostName = input.name;
        nixpkgs.hostPlatform = input.system;

        # Surfaces the source commit in `nixos-version --json`.
        system.configurationRevision = inputs.self.rev or inputs.self.dirtyRev or null;
      })

      nixos.unit
    ];
  });
}
