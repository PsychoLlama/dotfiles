{ config, lib, pkgs, ... }:

let cfg = config.presets.rofi;

in with lib; {
  options.presets.rofi.enable = mkEnableOption "Use the rofi launcher";

  config.programs.rofi = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.rofi;
    extraConfig.modi = "drun,run";

    theme = let inherit (config.lib.formats.rasi) mkLiteral;
    in {
      # Rofi theme. See `rofi-theme(5)` for details.
      # Adapted from taylor1791/dotfiles.

      # Global properties. Every element inherits these properties.
      "*" = {
        # OneDarkPro theme.
        black = mkLiteral "#1E1E1E";
        white = mkLiteral "#ABB2BF";
        red = mkLiteral "#E06C75";
        green = mkLiteral "#98C379";
        yellow = mkLiteral "#E5C07B";
        blue = mkLiteral "#61AFEF";
        magenta = mkLiteral "#C678DD";
        cyan = mkLiteral "#56B6C2";
        gutter-gray = mkLiteral "#5C6370";
        comment-gray = mkLiteral "#7D828D";

        # Derived-colors
        white10 = mkLiteral "#ABB2BF0A"; # @white 10/255 opaque

        background-color = mkLiteral "@black";
        font = "mononoki 16";
        text-color = mkLiteral "@white";
      };

      window = {
        border-color = mkLiteral "@gutter-gray";
        border = 2;
      };

      inputbar = {
        padding = mkLiteral "0.25em";
        border = mkLiteral "0 dash 0 dash 2px dash 0 dash";
        border-color = mkLiteral "@gutter-gray";
      };

      # To test an error message, use the following command:
      # $ rofi -e "Hello World"
      error-message = {
        background-color = mkLiteral "@magenta";
        text-color = mkLiteral "@black";
        padding = mkLiteral "1em";
      };

      textbox = {
        background-color = mkLiteral "inherit";
        text-color = mkLiteral "inherit";
      };

      # To test a message, use the following command:
      # $ echo 'a:b:c' | rofi -sep : -dmenu -mesg 'Choose an option:'
      message.padding = mkLiteral "0.25em";

      # It is hard not to test this with any rofi command. Example:
      # $ rofi -show drun
      listview = {
        background-color = mkLiteral "@black";
        scrollbar = true;
      };

      scrollbar = {
        handle-width = mkLiteral "0.5em";
        handle-color = mkLiteral "@gutter-gray";
      };

      element = {
        border = 0;
        padding = mkLiteral "0.125em";
      };

      "element.alternate.normal, element.alternate.active, element.alternate.urgent" =
        {
          background-color = mkLiteral "@white10";
        };

      # To test active states, run these commands:
      # $ 'a:b:c:d' | rofi -sep ':' -dmenu -a 2
      # $ 'a:b:c:d' | rofi -sep ':' -dmenu -a 3
      "element.normal.active, element.alternate.active" = {
        text-color = mkLiteral "@blue";
      };

      # To test active states, run this command:
      # $ 'a:b:c:d' | rofi -sep ':' -dmenu -u 2-3
      "element.normal.urgent, element.alternate.urgent" = {
        text-color = mkLiteral "@red";
      };

      # To test the selected states, run this command:
      # $ echo 'a,b,c,d' | rofi -sep , -dmenu -a 2 -u 3
      "element.selected.normal, element.selected.active, element.selected.urgent" =
        {
          text-color = mkLiteral "@black";
        };

      "element.selected.normal".background-color = mkLiteral "@white";
      "element.selected.active".background-color = mkLiteral "@blue";
      "element.selected.urgent".background-color = mkLiteral "@red";

      # I don't use the mode switcher; it is left unstyled.
    };
  };
}
