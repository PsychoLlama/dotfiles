{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  inherit (config.theme) palette;
  cfg = config.programs.presets.rofi;
in
{
  options.programs.presets.rofi.enable = mkEnableOption "Use the rofi launcher";

  config.programs.rofi = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.rofi;
    terminal = mkDefault "${config.programs.wezterm.package}/bin/wezterm";
    extraConfig.modi = "drun,run";

    theme =
      let
        inherit (config.lib.formats.rasi) mkLiteral;
      in
      {
        # Rofi theme. See `rofi-theme(5)` for details.
        # Adapted from taylor1791/dotfiles.

        "*" = {
          # TODO: Derive these from the theme.
          gutter-gray = mkLiteral "#5C6370";
          comment-gray = mkLiteral "#7D828D";
          white10 = mkLiteral "#ABB2BF0A"; # @white 10/255 opaque

          background-color = mkLiteral palette.normal.black;
          font = "Fira Code 16";
          text-color = mkLiteral palette.normal.white;
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
          background-color = mkLiteral palette.normal.magenta;
          text-color = mkLiteral palette.normal.black;
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
          background-color = mkLiteral palette.normal.black;
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

        "element.alternate.normal, element.alternate.active, element.alternate.urgent" = {
          background-color = mkLiteral "@white10";
        };

        # To test active states, run these commands:
        # $ 'a:b:c:d' | rofi -sep ':' -dmenu -a 2
        # $ 'a:b:c:d' | rofi -sep ':' -dmenu -a 3
        "element.normal.active, element.alternate.active" = {
          text-color = mkLiteral palette.normal.blue;
        };

        # To test active states, run this command:
        # $ 'a:b:c:d' | rofi -sep ':' -dmenu -u 2-3
        "element.normal.urgent, element.alternate.urgent" = {
          text-color = mkLiteral palette.normal.red;
        };

        # To test the selected states, run this command:
        # $ echo 'a,b,c,d' | rofi -sep , -dmenu -a 2 -u 3
        "element.selected.normal, element.selected.active, element.selected.urgent" = {
          text-color = mkLiteral palette.normal.black;
        };

        "element.selected.normal".background-color = mkLiteral palette.normal.white;
        "element.selected.active".background-color = mkLiteral palette.normal.blue;
        "element.selected.urgent".background-color = mkLiteral palette.normal.red;

        # I don't use the mode switcher; it is left unstyled.
      };
  };
}
