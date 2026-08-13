{
  imports = [ (import ./_mk-unstable-preset.nix "firefox") ];

  exports.homeManager =
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
    };
}
