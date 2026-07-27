{
  self,
  pkgs,
  ...
}:

let
  inherit (self.identity) username;
in

{
  # The hostname and the user's home directory are both owned by the NixOS
  # eval, so this block needs that eval's own `config`.
  platforms.nixos =
    { config, ... }:

    let
      inherit (config.home-manager.users.${username}.home) homeDirectory;
      inherit (config.networking) hostName;
    in

    {
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
          "${homeDirectory}/projects/@scratch"
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
      };
    };
}
