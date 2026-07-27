{
  self,
  pkgs,
  ...
}:

let
  inherit (self.identity) username;
in

{
  # The user's home directory is owned by the home-manager submodule inside the
  # NixOS eval, so this block needs that eval's own `config`.
  platforms.nixos =
    { config, ... }:

    let
      inherit (config.home-manager.users.${username}.home) homeDirectory;
    in

    {
      services.syncthing = {
        enable = true;

        # Use stable. New version expects flags in a different style.
        package = pkgs.syncthing;

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
              addresses = [ "tcp://nas-001" ];
              id = "SJZG6UN-EOBGJV6-RAV5X5F-6QBWY6U-AE5TXOT-2E57WP7-CAWQJRX-F3AO5QG";
            };

            phone = {
              addresses = [ "tcp://google-pixel-7" ];
              id = "7B5KM6T-7NXKMY5-KM7TIQJ-WFX2OBO-OHMZOPA-HAXTV5B-5RNKXFM-OEF5AAL";
            };
          };
        };
      };
    };
}
