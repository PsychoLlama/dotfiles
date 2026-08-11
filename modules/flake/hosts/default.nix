{
  config,
  inputs,
  lib,
  ...
}:

let
  inherit (lib) types;

  machines =
    system:
    lib.mkOption {
      description = "NixOS machines for this system, keyed by hostname.";
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
                type = types.str;
                readOnly = true;
                default = system;
              };
            };
          }
        )
      );
    };
in

{
  # The substrate reads flake inputs (`agenix`, `home-manager`, `self`), so it
  # is imported here rather than from a profile. Profiles are re-evaluated
  # inside a consumer's flake, where those inputs do not exist -- which is why
  # no preset references `inputs` either.
  imports = [ ../../system/substrate.nix ];

  options.rhizome.hosts = lib.mkOption {
    description = "NixOS machines, keyed by system then hostname.";
    default = { };

    # A submodule rather than `attrsOf`, which types values but not keys. One
    # option per supported system makes a typo'd double a missing-option error
    # instead of a phantom host.
    type = types.submodule { options = lib.genAttrs config.systems machines; };
  };

  config.flake.nixosConfigurations = lib.concatMapAttrs (
    _: hosts:
    lib.mapAttrs (
      _: host:
      inputs.nixpkgs.lib.nixosSystem {
        modules = [
          host.module
          config.flake.modules.nixos.default

          {
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
    ) hosts
  ) config.rhizome.hosts;
}
