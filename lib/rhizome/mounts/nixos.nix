{ lib, mkMount }:

# The nixos edition of the mount. Installs the plugins and ships the
# default routers: home-manager fragments ride `sharedModules` (when the
# home-manager module is present), and the hosted marker stops a nested
# standalone mount from installing the rhizome layer twice.
#
# Custom classes (e.g. an editor) are routed by ordinary user config:
# read `config.rhizome.fragments.<class>`, claim it in `rhizome.routed`.

plugins:

let
  hostedMarker = import ./hosted-marker.nix { inherit lib; };

  routers =
    { config, options, ... }:
    {
      config = lib.mkMerge (
        [
          {
            warnings = map (
              tag: "rhizome: fragments for class '${tag}' have no router and were dropped."
            ) config.rhizome.unrouted;
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
    (mkMount {
      class = "nixos";
      inherit plugins;
    })
    routers
  ];
}
