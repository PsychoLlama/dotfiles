{ rhizome, units, ... }:

let
  inherit (units.platform) home-manager;
in

rhizome.component "ssh-agent" [
  (home-manager.module (
    {
      config,
      lib,
      pkgs,
      ...
    }:

    {
      services.ssh-agent = {
        enable = true;
        package = pkgs.unstable.openssh;
      };
    }
  ))
]
