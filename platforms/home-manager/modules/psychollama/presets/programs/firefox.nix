{ config, lib, ... }:

let
  cfg = config.psychollama.presets.programs.firefox;
in

{
  config.programs.firefox = lib.mkIf cfg.enable {
    # Use the post-26.05 XDG profile location. The existing profile directory
    # was migrated from `~/.mozilla/firefox` to match.
    configPath = "${config.xdg.configHome}/mozilla/firefox";

    profiles.default = {
      isDefault = true;
      name = "default";
    };
  };
}
