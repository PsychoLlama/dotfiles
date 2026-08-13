{
  config,
  inputs,
  lib,
  ...
}:

let
  inherit (lib) types;
in

{
  # The substrate reads flake inputs (`agenix`, `home-manager`, `self`), so it
  # is imported here rather than from a profile. Profiles are re-evaluated
  # inside a consumer's flake, where those inputs do not exist -- which is why
  # no preset references `inputs` either.
  imports = [ ../aspects/system/substrate.nix ];

  options.rhizome.hosts = lib.mkOption {
    description = "NixOS machines, keyed by hostname.";
    default = { };

    type = types.attrsOf (
      types.submodule (
        { name, ... }:

        {
          options = {
            module = lib.mkOption {
              description = "The machine's NixOS configuration.";
              type = types.deferredModule;
              default = { };
            };

            name = lib.mkOption {
              description = "The machine's hostname.";
              type = types.str;
              readOnly = true;
              default = name;
            };

            system = lib.mkOption {
              description = "The machine's platform.";

              # An enum rather than `str`: a typo'd double should fail here,
              # not deep inside nixpkgs where the platform is finally used.
              type = types.enum config.systems;
            };
          };
        }
      )
    );
  };

  config.flake.nixosConfigurations = lib.mapAttrs (
    _: host:
    inputs.nixpkgs.lib.nixosSystem {
      modules = [
        host.module
        config.flake.modules.nixos.default

        {
          _module.args.host = host;

          networking.hostName = host.name;
          nixpkgs.hostPlatform = host.system;

          # Surface this flake's git revision in `nixos-version --json` so the
          # running system can be traced back to the source commit. Scoped to
          # hosts built here rather than `flake.modules.nixos.default`, which
          # downstream flakes import: they would stamp their systems with our
          # revision instead of their own.
          system.configurationRevision = inputs.self.rev or inputs.self.dirtyRev or null;
        }
      ];
    }
  ) config.rhizome.hosts;
}
