{ config, lib, pkgs, ... }:

with lib;

let cfg = config.presets.alacritty;

in {
  options.presets.alacritty.enable =
    mkEnableOption "Use the Alacritty terminal emulator";

  config = mkIf cfg.enable {
    programs.rofi.terminal = "alacritty";

    programs.alacritty = {
      enable = true;
      package = pkgs.unstable.alacritty;

      settings = {
        env.TERM = "xterm-256color";

        window = {
          # Disable borders and the title bar.
          decorations = "none";

          # Open fullscreen by default.
          startup_mode = "Fullscreen";

          # Default window size in columns/lines.
          dimensions = {
            columns = 80;
            lines = 24;
          };

          # Window padding.
          padding = {
            x = 2;
            y = 2;
          };
        };

        # Scrollback buffers belong to the multiplexer.
        scrolling.history = 0;

        # Default is too small.
        font.size = 14;

        # Preferably `hide_forever`, but people seem to like it.
        mouse.hide_when_typing = true;

        # Start tmux automatically.
        shell.program = "${pkgs.tmux}/bin/tmux";

        # Source: OneDarkPro.nvim
        # TODO: Extract this into a color palette module.
        colors = {
          primary = {
            background = "#1e1e1e";
            foreground = "#abb2bf";
          };

          normal = {
            black = "#1e1e1e";
            red = "#e06c75";
            green = "#98c379";
            yellow = "#e5c07b";
            blue = "#61afef";
            magenta = "#c678dd";
            cyan = "#56b6c2";
            white = "#abb2bf";
          };

          bright = {
            black = "#3f3f3f";
            red = "#e06c75";
            green = "#98c379";
            yellow = "#e5c07b";
            blue = "#61afef";
            magenta = "#c678dd";
            cyan = "#56b6c2";
            white = "#bfc5ce";
          };

          cursor = {
            cursor = "CellForeground";
            text = "CellBackground";
          };

          selection = {
            background = "#3e4452";
            text = "CellForeground";
          };
        };
      };
    };
  };
}

