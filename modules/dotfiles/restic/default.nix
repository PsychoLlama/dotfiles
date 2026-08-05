{
  dotfiles.restic =
    { user, ... }:

    {
      nixos =
        { config, pkgs, ... }:

        let
          inherit (config.users.users.${user.userName}) home;
          inherit (config.networking) hostName;
        in

        {
          age.secrets.restic-env.file = ./env.age;

          services.restic.backups.${user.userName} = {
            repository = "rest:https://restic.selfhosted.city/workstation-${hostName}/";
            initialize = true;
            environmentFile = config.age.secrets.restic-env.path;
            package = pkgs.unstable.restic;

            paths = [
              home
              "${home}/projects/psychollama"
              "${home}/projects/taylor1791"
              "${home}/projects/@scratch"
            ];

            exclude = [
              # Exclude all projects (specific ones added via paths above)
              "${home}/projects"

              # Caches and temp
              "${home}/.cache"
              "${home}/.local/share/Trash"
              "${home}/.npm"
              "${home}/.cargo/registry"
              "${home}/.cargo/git"

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
