{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (config.theme) palette;
  cfg = config.psychollama.presets.programs.wezterm;
in

{
  config.programs.wezterm = lib.mkIf cfg.enable {
    configFile = ./wezterm.lua;

    settings = {
      # Assumes `pkgs.wezterm.terminfo` exists in `$TERMINFO_DIRS`.
      term = "wezterm";

      color_scheme = "OneDarkPro";
      window_background_opacity = 0.85;
      hide_tab_bar_if_only_one_tab = true;
      font_size = 14;

      font = {
        family = "FiraCode Nerd Font";
        weight = "Light";
      };

      # Disable ligatures.
      harfbuzz_features = [
        "calt=0"
        "clig=0"
        "liga=0"
      ];

      window_padding = {
        left = 0;
        right = 0;
        top = 0;
        bottom = 0;
      };

      default_prog = [
        (toString (pkgs.writeScript "start-tmux" ''
          first_session="$(
            ${pkgs.tmux}/bin/tmux list-sessions \
            -F '#{session_id}' 2>/dev/null \
            | head -1 || true
          )"

          if [[ -n "$first_session" ]]; then
            ${pkgs.tmux}/bin/tmux attach-session -t "$first_session"
          else
            ${pkgs.tmux}/bin/tmux new-session
          fi
        ''))
      ];

      colors = {
        ansi = [
          palette.normal.black
          palette.normal.red
          palette.normal.green
          palette.normal.yellow
          palette.normal.blue
          palette.normal.magenta
          palette.normal.cyan
          palette.normal.white
        ];

        brights = [
          palette.bright.black
          palette.bright.red
          palette.bright.green
          palette.bright.yellow
          palette.bright.blue
          palette.bright.magenta
          palette.bright.cyan
          palette.bright.white
        ];

        background = palette.normal.black;
        foreground = palette.normal.white;

        selection_bg = palette.bright.black;
        selection_fg = palette.normal.black;

        cursor_bg = palette.normal.white;
        cursor_border = palette.normal.white;
        cursor_fg = palette.normal.black;
      };
    };
  };
}
