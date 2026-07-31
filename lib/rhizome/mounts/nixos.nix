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

let
  hostedMarker = import ./hosted-marker.nix { inherit lib; };

  routers =
    { config, options, ... }:
    {
      config = lib.mkMerge (
        [
          {
            # A stack has exactly one OS host, so darwin fragments could
            # never apply here. That is a property of this root, not of
            # the class table — the darwin root drops nixos in turn.
            rhizome.dropped = [ "darwin" ];

            assertions = lib.map (tag: {
              assertion = false;
              message = "rhizome: class '${tag}' produced fragments, but no router claimed them. Carry it with `rhizome.routed = [ \"${tag}\" ];` after wiring the fragments into a `${tag}` eval, or discard it deliberately with `rhizome.dropped = [ \"${tag}\" ];`.";
            }) config.rhizome.unrouted;
          }
        ]
        ++ lib.optionals (options ? home-manager) [
          {
            rhizome.routed = [ "homeManager" ];
            home-manager.sharedModules = config.rhizome.fragments.homeManager ++ [ hostedMarker ];
          }
        ]
      );
    };
in

{
  imports = [
    (mount {
      class = "nixos";
      inherit plugins;
    })
    routers
  ];
}
