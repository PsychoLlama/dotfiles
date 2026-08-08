{ config, lib, ... }:

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
  options.hosts = lib.mkOption {
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
      config.flake.lib.hosts.nixos {
        imports = [ host.module ];
        networking.hostName = host.name;
        nixpkgs.hostPlatform = host.system;
      }
    ) hosts
  ) config.hosts;
}
