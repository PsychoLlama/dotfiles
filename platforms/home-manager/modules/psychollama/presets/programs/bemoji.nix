{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.programs.bemoji;
in

{
  options.psychollama.presets.programs.bemoji = {
    enable = lib.mkEnableOption "Emoji picker using fuzzel";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.unstable.bemoji
      pkgs.unstable.wtype
    ];

    home.sessionVariables.BEMOJI_PICKER_CMD = "fuzzel -d";
  };
}
