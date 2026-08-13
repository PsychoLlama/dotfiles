{
  config,
  inputs,
  lib,
  ...
}:

let
  inherit (lib) types;

  # The host submodule shadows `config` with its own.
  defaults = config.rhizome.defaults;
  systems = config.systems;
  nixosModules = config.flake.nixosModules;
in

{
  options.rhizome.defaults.host = lib.mkOption {
    description = ''
      Configuration folded into every host's `module`, so it survives a
      custom `builder`. Reads the machine through a `host` module argument.
    '';

    type = types.deferredModule;
    default = { };
  };

  options.rhizome.hosts = lib.mkOption {
    description = "Machines, keyed by hostname.";
    default = { };

    # `submoduleWith` so a host can import the options it wants from `hosts/_*.nix`.
    type = types.attrsOf (
      types.submoduleWith {
        modules = [
          (
            { name, config, ... }:

            {
              options = {
                builder = lib.mkOption {
                  description = ''
                    Turns the host into whatever a machine is on its platform.
                    Override it for a host this flake's `nixpkgs` cannot build --
                    `darwinSystem`, a pinned nixpkgs, an image builder.
                  '';

                  type = types.functionTo types.raw;

                  default =
                    host:
                    inputs.nixpkgs.lib.nixosSystem {
                      modules = [
                        host.module
                        nixosModules.default
                      ]
                      ++ map (id: nixosModules.${id}) host.profiles;
                    };
                };

                install = lib.mkOption {
                  description = ''
                    Flake outputs the host contributes, merged with every other
                    host's. Written relative to `flake`, and config only -- no
                    `options`, no `imports`.

                    Override it to publish somewhere other than
                    `nixosConfigurations`, or to publish to more than one place.
                  '';

                  type = types.functionTo types.raw;
                  default = host: { nixosConfigurations.${host.name} = host.output; };
                };

                module = lib.mkOption {
                  description = "The machine's own configuration.";
                  default = { };

                  type = types.deferredModuleWith {
                    staticModules = [
                      { _module.args.host = config; }
                      defaults.host
                    ];
                  };
                };

                name = lib.mkOption {
                  description = "The machine's hostname.";
                  type = types.str;
                  readOnly = true;
                  default = name;
                };

                output = lib.mkOption {
                  description = "The built machine, as `install` publishes it.";
                  type = types.raw;
                  readOnly = true;
                  default = config.builder config;
                };

                profiles = lib.mkOption {
                  description = "Aspects applied to the machine, by id.";
                  default = [ ];

                  # An enum so a typo fails here, naming the host and the bad id.
                  type = types.listOf (types.enum (lib.attrNames nixosModules));
                };

                system = lib.mkOption {
                  description = "The machine's platform.";
                  type = types.enum systems;
                };
              };
            }
          )
        ];
      }
    );
  };

  config.rhizome.defaults.host =
    { host, ... }:

    {
      networking.hostName = host.name;
      nixpkgs.hostPlatform = host.system;

      # Surfaces the source commit in `nixos-version --json`. Per-host, so a
      # downstream flake stamps its own.
      system.configurationRevision = inputs.self.rev or inputs.self.dirtyRev or null;
    };

  # Rooted at `flake`: at the module root this recurses through `rhizome.hosts`.
  config.flake = lib.mkMerge (lib.mapAttrsToList (_: host: host.install host) config.rhizome.hosts);
}
