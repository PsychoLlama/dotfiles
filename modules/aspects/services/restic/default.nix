{ config, ... }:

# Bound out here because the module below shadows `config` with its own.
let
  inherit (config.identity) username;
in

{
  imports = [ ../../../rhizome/identity.nix ];

  flake.modules.nixos.default =
    {
      config,
      lib,
      pkgs,
      ...
    }:

    let
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
    };
}
