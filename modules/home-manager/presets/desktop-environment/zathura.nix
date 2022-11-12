{ config, lib, pkgs, ... }:

with lib;

let cfg = config.presets.zathura;

in {
  options.presets.zathura.enable =
    mkEnableOption "Install and configure Zathura";

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

        default-bg = "#282C34";
        default-fg = "#ABB2BF";
        statusbar-bg = "#2C323C";
        statusbar-fg = "#ABB2BF";
        inputbar-bg = "#282C34";
        inputbar-fg = "#ABB2BF";

        # To test, use tab completion. E.g :open <TAB>
        completion-bg = "#2C323C";
        completion-fg = "#ABB2BF";
        completion-group-bg = "#2C323C";
        completion-group-fg = "#ABB2BF";
        completion-highlight-fg = "#2C323C";
        completion-highlight-bg = "#61AFEF";

        render-loading-bg = "#2C323C";
        render-loading-fg = "#61AFEF";

        # To test, search for text
        highlight-color = "#61AFEF";
        highlight-active-color = "#C678DD";

        # To test, open the index with <TAB>.
        index-bg = "#282C34";
        index-fg = "#ABB2BF";
        index-active-bg = "#61AFEF";
        index-active-fg = "#2C323C";

        # To test, select some text with the mouse.
        notification-bg = "#98C379";
        notification-fg = "#282C34";

        notification-warning-bg = "#E5C07B";
        notification-warning-fg = "#282C34";

        # To test, set an unkown option. E.g :set chicken fried
        notification-error-bg = "#E06C75";
        notification-error-fg = "#282C34";

        # Recolor the pdfs to match OneDark. Impressive.
        recolor = true;
        recolor-keephue = true;
        recolor-darkcolor = "#ABB2BF";
        recolor-lightcolor = "#282C34";
      };
    };
  };
}
