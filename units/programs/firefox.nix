{ rhizome, units, ... }:

let
  inherit (units.platform) home-manager;
in

rhizome.unit "firefox" {
  unit = rhizome.component [
    (home-manager.program "firefox")

    (home-manager.module (
      { config, ... }:

      {
        programs.firefox = {
          # Use the post-26.05 XDG profile location. The existing profile directory
          # was migrated from `~/.mozilla/firefox` to match.
          configPath = "${config.xdg.configHome}/mozilla/firefox";

          profiles.default = {
            isDefault = true;
            name = "default";
          };
        };
      }
    ))
  ];
}
