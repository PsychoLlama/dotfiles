{ rhizome, units, ... }:

let
  inherit (units.platform) home-manager;
in

rhizome.unit "ssh-agent" {
  unit = rhizome.component [
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
  ];
}
