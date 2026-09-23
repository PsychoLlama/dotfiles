{
  exports.nixos =
    {
      config,
      host,
      pkgs,
      ...
    }:

    let
      inherit (host.identity) username;
      inherit (config.home-manager.users.${username}.home) homeDirectory;
      inherit (config.networking) hostName;
    in

    {
      config = {
        age.secrets.restic-env.file = ./env.age;

        services.restic.backups.${username} = {
          repository = "rest:https://restic.selfhosted.city/workstation-${hostName}/";
          initialize = true;
          environmentFile = config.age.secrets.restic-env.path;
          package = pkgs.unstable.restic;

          paths = [
            homeDirectory
            "/root"
            "/var/log"
          ];

          exclude = [
            # Exclude all projects except these. Must match children, not the
            # parent, or the negations never apply.
            "${homeDirectory}/projects/*"
            "!${homeDirectory}/projects/psychollama"
            "!${homeDirectory}/projects/taylor1791"
            "!${homeDirectory}/projects/@scratch"

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
        };
      };
    };
}
