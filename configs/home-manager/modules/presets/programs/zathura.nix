{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  inherit (config.theme) palette;
  cfg = config.presets.programs.zathura;
in
{
  options.presets.programs.zathura.enable = mkEnableOption "Install and configure Zathura";

  config = mkIf cfg.enable {
    programs.zathura = {
      enable = true;
      package = pkgs.unstable.zathura;

      # Adapted from taylor1791/dotfiles. Thanks Taylor.
      options = {
        # A configuration file for zathura. See `man zathurarc` for more details.

        # Use clipboard instead of primary.
        selection-clipboard = "clipboard";

        # Replace $HOME with ~ in the window title.
        window-title-home-tilde = true;

        # Only show the basename in the statusbar.
        statusbar-basename = true;

        # A OneDark theme for zathura
        font = "Fira Code 13";

        default-bg = palette.normal.black;
        default-fg = palette.normal.white;
        statusbar-bg = palette.bright.black;
        statusbar-fg = palette.normal.white;
        inputbar-bg = palette.normal.black;
        inputbar-fg = palette.normal.white;

        # To test, use tab completion. E.g :open <TAB>
        completion-bg = palette.bright.black;
        completion-fg = palette.normal.white;
        completion-group-bg = palette.bright.black;
        completion-group-fg = palette.normal.white;
        completion-highlight-fg = palette.bright.black;
        completion-highlight-bg = palette.normal.blue;

        render-loading-bg = palette.bright.black;
        render-loading-fg = palette.normal.blue;

        # To test, search for text
        highlight-color = palette.normal.blue;
        highlight-active-color = palette.normal.magenta;

        # To test, open the index with <TAB>.
        index-bg = palette.normal.black;
        index-fg = palette.normal.white;
        index-active-bg = palette.normal.blue;
        index-active-fg = palette.bright.black;

        # To test, select some text with the mouse.
        notification-bg = palette.normal.green;
        notification-fg = palette.normal.black;

        notification-warning-bg = palette.normal.yellow;
        notification-warning-fg = palette.normal.black;

        # To test, set an unkown option. E.g :set chicken fried
        notification-error-bg = palette.normal.red;
        notification-error-fg = palette.normal.black;

        # Recolor the pdfs to match OneDark. Impressive.
        recolor = true;
        recolor-keephue = true;
        recolor-darkcolor = palette.normal.white;
        recolor-lightcolor = palette.normal.black;
      };
    };
  };
}
