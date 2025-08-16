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
          addresses = [ "tcp://rpi3-002.host.selfhosted.city" ];
          id = "MLM3RUS-6LHM76Q-OPW5UIC-EAH7EUM-ZNG6TJW-TDASURZ-GCZ2YOX-ASNI6Q4";
        };

        phone = {
          addresses = [ "dynamic" ];
          id = "7B5KM6T-7NXKMY5-KM7TIQJ-WFX2OBO-OHMZOPA-HAXTV5B-5RNKXFM-OEF5AAL";
        };
      };
    };
  };
}
