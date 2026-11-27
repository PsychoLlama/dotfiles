# A machine, and what is true of all of it.
{
  types,
  units,
  inputs,
  self,
  define,
  ...
}:

let
  t = types;
  inherit (units.platform) nixos;

  fact =
    type:
    define.fact {
      scope = self.scope;
      inherit type;
      allow-unread = true;
    };
in

{
  scope = define.scope { };

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
