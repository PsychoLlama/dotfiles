{
  config,
  inputs,
  lib,
  ...
}:

let
  inherit (lib) types;

  # Bound here because the host submodule shadows `config` with its own.
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

    type = types.attrsOf (
      types.submodule (
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

                    { _module.args.host = host; }
                  ]
                  ++ map (id: nixosModules.${id}) host.profiles;
                };
            };

            install = lib.mkOption {
              description = ''
                Flake outputs the host contributes, merged with every other
                host's. Only the `flake` attribute is read, and only its
                config -- no `options`, no `imports`.

                Override it to publish somewhere other than
                `nixosConfigurations`, or to publish to more than one place.
              '';

              type = types.functionTo types.raw;
              default = host: { flake.nixosConfigurations.${host.name} = host.output; };
            };

            module = lib.mkOption {
              description = "The machine's own configuration.";
              type = types.deferredModuleWith { staticModules = [ defaults.host ]; };
              default = { };
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

              # An enum for the same reason `system` is one: the failure should
              # name the host and the misspelled id, not surface as a missing
              # attribute wherever the module is finally looked up.
              type = types.listOf (types.enum (lib.attrNames nixosModules));
            };

            system = lib.mkOption {
              description = "The machine's platform.";

              # An enum rather than `str`: a typo'd double should fail here,
              # not deep inside nixpkgs where the platform is finally used.
              type = types.enum systems;
            };
          };
        }
      )
    );
  };

  config.rhizome.defaults.host =
    { host, ... }:

    {
      networking.hostName = host.name;
      nixpkgs.hostPlatform = host.system;

      # Surface this flake's git revision in `nixos-version --json` so the
      # running system can be traced back to the source commit. Scoped to hosts
      # built here rather than `flake.nixosModules.default`, which downstream
      # flakes import: they would stamp their systems with our revision instead
      # of their own.
      system.configurationRevision = inputs.self.rev or inputs.self.dirtyRev or null;
    };

  # Rooted at `flake` rather than the module root, which recurses: the root
  # would have to evaluate every `install` to find out which options it defines,
  # and `rhizome.hosts` is one of them.
  config.flake = lib.mkMerge (
    lib.mapAttrsToList (_: host: (host.install host).flake or { }) config.rhizome.hosts
  );
}
