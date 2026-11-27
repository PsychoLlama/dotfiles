{ units, module, ... }:

let
  inherit (units.platform) home-manager;
in

{
  unit = module.component "unit" {
    config = [
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
  };
}
