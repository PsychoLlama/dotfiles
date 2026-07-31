{ lib, mount }:

/**
  The standalone home-manager edition of the mount, for hosts where
  home-manager *is* the top-level host (WSL, foreign distros). Refuses to
  evaluate under an OS host that already manages this configuration — the
  rhizome layer must evaluate exactly once per stack.

  # Type

  ```
  mounts.home-manager :: AttrSet Plugin -> Module
  ```
*/

plugins:

let
  guard =
    { config, ... }:
    {
      config = {
        # Home-manager is the top-level host here, so neither OS class can
        # apply. Both are discarded by this root rather than by the class
        # table, which stays free of stack policy.
        rhizome.dropped = [
          "nixos"
          "darwin"
        ];

        assertions = [
          {
            assertion = !(config.rhizome.hosted or false);
            message = "rhizome: this home-manager configuration is already managed by the OS host above it. Remove the standalone home-manager mount.";
          }
        ]
        ++ lib.map (tag: {
          assertion = false;
          message = "rhizome: class '${tag}' produced fragments, but no router claimed them. Carry it with `rhizome.routed = [ \"${tag}\" ];` after wiring the fragments into a `${tag}` eval, or discard it deliberately with `rhizome.dropped = [ \"${tag}\" ];`.";
        }) config.rhizome.unrouted;
      };
    };
in

{
  imports = [
    (mount {
      class = "homeManager";
      inherit plugins;
    })
    guard
  ];
}
