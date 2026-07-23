{ lib, mkRoot }:

# The nixos edition of the root guest. Mounts the plugins and ships the
# default installers: home-manager fragments ride `sharedModules` (when
# the home-manager module is present), and the hosted marker stops a
# nested standalone guest from double-mounting the meta layer.
#
# Custom classes (e.g. an editor) are routed by ordinary user config:
# read `config._meta.fragments.<class>`, claim it in `_meta.routed`.

plugins:

let
  hostedMarker = import ./hosted-marker.nix { inherit lib; };

  installers =
    { config, options, ... }:
    {
      config = lib.mkMerge (
        [
          {
            warnings = map (
              tag: "module system: fragments for class '${tag}' have no installer and were dropped."
            ) config._meta.unrouted;
          }
        ]
        ++ lib.optionals (options ? home-manager) [
          {
            _meta.routed = [ "homeManager" ];
            home-manager.sharedModules = config._meta.fragments.homeManager ++ [ hostedMarker ];
          }
        ]
      );
    };
in

{
  imports = [
    (mkRoot {
      class = "nixos";
      inherit plugins;
    })
    installers
  ];
}
