{ lib, mkRoot }:

# The standalone home-manager edition of the root guest, for hosts where
# home-manager *is* the top-level root (WSL, foreign distros). Refuses to
# evaluate under an OS root that already manages this configuration —
# the meta layer must evaluate exactly once per stack.

plugins:

let
  guard =
    { config, ... }:
    {
      config = {
        assertions = [
          {
            assertion = !(config._meta.hosted or false);
            message = "module system: this home-manager configuration is already managed by the OS root above it. Remove the standalone home-manager guest.";
          }
        ];

        warnings = map (
          tag: "module system: fragments for class '${tag}' have no installer and were dropped."
        ) config._meta.unrouted;
      };
    };
in

{
  imports = [
    (mkRoot {
      class = "homeManager";
      inherit plugins;
    })
    guard
  ];
}
