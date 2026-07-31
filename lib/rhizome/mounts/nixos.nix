{ lib, mount }:

/**
  The nixos edition of the mount. Installs the plugins and ships the
  default routers: home-manager fragments ride `sharedModules` (when the
  home-manager module is present), and the hosted marker stops a nested
  standalone mount from installing the rhizome layer twice.

  Custom classes (e.g. an editor) are routed by ordinary user config:
  read `config.rhizome.fragments.<class>`, claim it in `rhizome.routed`.
  A class this root neither carries nor knowingly discards is a failure —
  it means a module's configuration was written and then went nowhere.

  # Type

  ```
  mounts.nixos :: AttrSet Plugin -> Module
  ```
*/

plugins:

mount {
  class = "nixos";
  inherit plugins;

  configure =
    { config, options, ... }:
    {
      config = lib.mkMerge (
        [
          {
            # A stack has exactly one OS host, so darwin fragments could
            # never apply here. That is a property of this root, not of
            # the class table — the darwin root drops nixos in turn.
            rhizome.dropped = [ "darwin" ];
          }
        ]
        ++ lib.optionals (options ? home-manager) [
          {
            rhizome.routed = [ "homeManager" ];

            home-manager.sharedModules = config.rhizome.fragments.homeManager ++ [
              # Marks the layer below as already managed from up here, so
              # the standalone home-manager mount can refuse to install a
              # second rhizome layer. Read-only, so a nested configuration
              # cannot talk its way out of being hosted: the default is a
              # definition, and an override would be the second one.
              {
                options.rhizome.hosted = lib.mkOption {
                  type = lib.types.bool;
                  default = true;
                  readOnly = true;
                  internal = true;
                  description = "Whether an outer eval's rhizome layer already manages this configuration.";
                };
              }
            ];
          }
        ]
      );
    };
}
