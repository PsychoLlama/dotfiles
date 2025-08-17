{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (config.psychollama.settings) username;
  inherit (config.home-manager.users.${username}.home) homeDirectory;
  cfg = config.psychollama.presets.services.syncthing;
in

{
  options.psychollama.presets.services.syncthing = {
    enable = lib.mkEnableOption "Sync files with Syncthing";
  };

  config.services.syncthing = lib.mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.syncthing;

    user = username;
    group = "users";
    dataDir = homeDirectory;

    settings = {
      options.urAccepted = 3;
      gui.theme = "dark";

      # A general-purpose box for reliable storage.
      folders."${homeDirectory}/attic" = {
        id = "attic";
        label = "Attic";
        devices = [
          "file-server"
          "phone"
        ];
      };

      devices = {
        file-server = {
          addresses = [ "tcp://nas-001.host.nova.selfhosted.city" ];
          id = "SJZG6UN-EOBGJV6-RAV5X5F-6QBWY6U-AE5TXOT-2E57WP7-CAWQJRX-F3AO5QG";
        };

        phone = {
          addresses = [ "dynamic" ];
          id = "7B5KM6T-7NXKMY5-KM7TIQJ-WFX2OBO-OHMZOPA-HAXTV5B-5RNKXFM-OEF5AAL";
        };
      };
    };
  };
}
