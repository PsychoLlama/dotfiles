{ config, lib, pkgs, ... }:

with lib;

let cfg = config.presets.ncspot;

in {
  options.presets.ncspot.enable = mkEnableOption "Install and configure ncspot";

  config.programs.ncspot = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.ncspot;

    settings = {
      flip_status_indicator = true;
      library_tabs = [ "playlists" "albums" "artists" "tracks" "browse" ];
      repeat = "off";
      shuffle = false;
      use_nerdfont = true;

      keybindings = {
        g = "move top";
        G = "move bottom";
        "Ctrl+d" = "move down 20";
        "Ctrl+u" = "move up 20";
        "Ctrl+e" = "move down 1";
        "Ctrl+y" = "move up 1";

        # Delete - it's too dangerous to be left alive.
        d = "noop";
        q = "back";
      };

      # Gruvbox dark.
      # TODO: Use One Dark theme.
      theme = {
        background = "default";
        primary = "#a89984";
        secondary = "#928374";
        title = "#8ec07c";
        playing = "#689d6a";
        playing_bg = "#383838";
        playing_selected = "#ebdbb2";
        highlight = "#d5c4a1";
        highlight_bg = "#484848";
        error = "#fbf1c7";
        error_bg = "#cc241d";
        statusbar_progress = "#458588";
        statusbar_bg = "#282828";
        statusbar = "#98971a";
        cmdline = "#d5c4a1";
        cmdline_bg = "#383838";
        search_match = "#fabd2f";
      };
    };
  };
}
