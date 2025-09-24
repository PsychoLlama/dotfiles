{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (config.theme) palette;
  cfg = config.psychollama.presets.programs.fuzzel;
  rgba = hex: alpha: "${lib.substring 1 (-1) hex}${lib.toHexString (builtins.ceil (alpha * 255))}";
  opaque = hex: rgba hex 1.0;

  textColor = palette.normal.white;
  accentColor = palette.normal.blue;
in

{
  options.psychollama.presets.programs.fuzzel = {
    enable = lib.mkEnableOption "Install the latest version of fuzzel";
  };

  config.programs.fuzzel = lib.mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.fuzzel;
    settings = {
      main = {
        horizontal-pad = 16;
        vertical-pad = 8;
        inner-pad = 8;
        match-counter = "yes";
      };

      border = {
        radius = 4;
        width = 0;
        selection-radius = 2;
      };

      colors = {
        background = rgba palette.normal.black 0.8;
        text = opaque textColor;
        prompt = opaque palette.bright.black;
        placeholder = opaque textColor;
        input = opaque textColor;
        match = opaque accentColor;
        selection = rgba palette.bright.black 0.8;
        selection-text = opaque palette.normal.white;
        selection-match = opaque accentColor;
        counter = opaque palette.bright.black;
        border = opaque palette.normal.black;
      };
    };
  };
}
