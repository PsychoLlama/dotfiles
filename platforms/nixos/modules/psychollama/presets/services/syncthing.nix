{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (config.home-manager.users.${cfg.username}.home) homeDirectory;
  cfg = config.psychollama.presets.services.syncthing;
in

{
  options.psychollama.presets.services.syncthing = {
    enable = lib.mkEnableOption "Sync files with Syncthing";
    username = lib.mkOption {
      type = lib.types.str;
      description = ''
        The user under which files are created and owned
      '';
    };
  };

  config.services.syncthing = lib.mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.syncthing;

    user = cfg.username;
    group = "users";
    dataDir = homeDirectory;

    settings = {
      options.urAccepted = 3;
      gui.theme = "dark";

      # A general-purpose box for reliable storage.
      folders."${homeDirectory}/attic" = {
        id = "attic";
        devices = [
          "file-server"
          "phone"
        ];
        label = "Attic";
      };

      devices = {
        file-server = {
          addresses = [ "tcp://rpi3-002.host.selfhosted.city" ];
          id = "MLM3RUS-6LHM76Q-OPW5UIC-EAH7EUM-ZNG6TJW-TDASURZ-GCZ2YOX-ASNI6Q4";
        };

        phone = {
          addresses = [ "dynamic" ];
          id = "S2U7KKV-SXJGOI3-6MSJWIT-U2JP32Y-HH7WZU5-ZDS6KAT-6CNYRAM-ZQTWZAQ";
        };
      };
    };
  };
}
