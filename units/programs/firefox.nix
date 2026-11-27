{ units, module, ... }:

let
  inherit (units.platform) home-manager;
in

{
  unit = module.component "unit" {
    config = [
      (home-manager.lib.program "firefox")

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
  };
}
