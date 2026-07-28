{ lib, pkgs, ... }:

{
  modules.home-manager =
    { config, ... }:

    {
      programs.firefox = {
        enable = lib.mkDefault true;
        package = lib.mkDefault pkgs.unstable.firefox;

        # Use the post-26.05 XDG profile location. The existing profile directory
        # was migrated from `~/.mozilla/firefox` to match.
        configPath = "${config.xdg.configHome}/mozilla/firefox";

        profiles.default = {
          isDefault = true;
          name = "default";
        };
      };
    };
}
