{ lib, mkMount }:

# The standalone home-manager edition of the mount, for hosts where
# home-manager *is* the top-level host (WSL, foreign distros). Refuses to
# evaluate under an OS host that already manages this configuration — the
# rhizome layer must evaluate exactly once per stack.

plugins:

let
  guard =
    { config, ... }:
    {
      config = {
        assertions = [
          {
            assertion = !(config.rhizome.hosted or false);
            message = "rhizome: this home-manager configuration is already managed by the OS host above it. Remove the standalone home-manager mount.";
          }
        ];

        warnings = map (
          tag: "rhizome: fragments for class '${tag}' have no router and were dropped."
        ) config.rhizome.unrouted;
      };
    };
in

{
  imports = [
    (mkMount {
      class = "homeManager";
      inherit plugins;
    })
    guard
  ];
}
