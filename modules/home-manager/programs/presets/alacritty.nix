{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.presets.alacritty;
  inherit (config.theme) palette;

in {
  options.programs.presets.alacritty.enable =
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

          opacity = 0.85;

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
        font.size = if pkgs.stdenv.isDarwin then 16 else 14;

        # Preferably `hide_forever`, but people seem to like it.
        mouse.hide_when_typing = true;

        # Start tmux automatically.
        shell.program = "${pkgs.tmux}/bin/tmux";

        colors = {
          inherit (palette) normal bright;

          primary = {
            background = palette.normal.black;
            foreground = palette.normal.white;
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

        # Achieve some normalcy while switching between macOS and Linux.
        keybindings = optionals pkgs.stdenv.isDarwin [
          {
            key = "C";
            mods = "Control|Shift";
            action = "Copy";
          }
          {
            key = "V";
            mods = "Control|Shift";
            action = "Paste";
          }
        ];
      };
    };
  };
}

