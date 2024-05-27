{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  inherit (config.theme) palette;
  cfg = config.programs.presets.wezterm;
  inline = generators.mkLuaInline;
  toLua = generators.toLua { };
  settings = {
    color_scheme = "OneDarkPro";
    window_background_opacity = 0.85;
    hide_tab_bar_if_only_one_tab = true;

    font = inline "wezterm.font('Fira Code', { weight = 'Light' })";
    font_size = 14;

    # Disable ligatures.
    harfbuzz_features = [ "calt=0" "clig=0" "liga=0" ];

    window_padding = {
      left = 0;
      right = 0;
      top = 0;
      bottom = 0;
    };

    default_prog = map toString [
      (pkgs.writeScript "start-tmux" ''
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
      '')
    ];

    keys = [
      {
        key = "L";
        mods = "CTRL";
        action = inline "wezterm.action.ShowDebugOverlay";
      }

      {
        key = "C";
        mods = "CTRL|SHIFT";
        action = inline "wezterm.action.CopyTo('Clipboard')";
      }

      {
        key = "V";
        mods = "CTRL|SHIFT";
        action = inline "wezterm.action.PasteFrom('Clipboard')";
      }
    ];
  };
in
{
  options.programs.presets.wezterm.enable = mkEnableOption "Install and configure wezterm";

  config.programs.wezterm = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.wezterm;

    colorSchemes.OneDarkPro = {
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

    extraConfig = ''
      local wezterm = require('wezterm')

      return ${toLua settings};
    '';
  };
}
