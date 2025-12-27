{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (config.psychollama.settings) username;
  inherit (config.home-manager.users.${username}.home) homeDirectory;
  inherit (config.networking) hostName;
  cfg = config.psychollama.presets.services.restic;
in

{
  options.psychollama.presets.services.restic = {
    enable = lib.mkEnableOption "Automated backups to home NAS via Restic";
  };

  config = lib.mkIf cfg.enable {
    age.secrets.restic-env.file = ./env.age;

    services.restic.backups.home = {
      repository = "rest:https://restic.selfhosted.city/workstation-${hostName}/";
      initialize = true;
      environmentFile = config.age.secrets.restic-env.path;
      package = pkgs.unstable.restic;

      paths = [
        homeDirectory
        "${homeDirectory}/projects/psychollama"
        "${homeDirectory}/projects/taylor1791"
      ];

      exclude = [
        # Exclude all projects (specific ones added via paths above)
        "${homeDirectory}/projects"

        # Caches and temp
        "${homeDirectory}/.cache"
        "${homeDirectory}/.local/share/Trash"
        "${homeDirectory}/.npm"
        "${homeDirectory}/.cargo/registry"
        "${homeDirectory}/.cargo/git"

        # Build artifacts
        "**/node_modules"
        "**/target/debug"
        "**/target/release"
        "**/dist/"
        "**/.direnv"
        "**/result"

        # Git object store (working tree still backed up)
        "**/.git/objects"
        "**/.git/lfs"
      ];

      timerConfig = {
        OnCalendar = "daily";
        Persistent = true;
        RandomizedDelaySec = "1h";
      };

      pruneOpts = [
        "--keep-daily 7"
        "--keep-weekly 4"
        "--keep-monthly 6"
      ];
    };
  };
}
