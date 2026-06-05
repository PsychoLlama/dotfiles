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
    programs.bemoji = {
      enable = true;
      package = pkgs.unstable.bemoji;
    };

    # bemoji uses wtype to type the selected emoji into the focused window.
    programs.wtype = {
      enable = true;
      package = pkgs.unstable.wtype;
    };

    home.sessionVariables.BEMOJI_PICKER_CMD = "fuzzel -d";
  };
}
