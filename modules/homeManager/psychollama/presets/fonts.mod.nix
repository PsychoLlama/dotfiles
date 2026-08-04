{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.fonts;
in

{
  options.psychollama.presets.fonts.enable = lib.mkEnableOption "Use recommended fonts";

  config = lib.mkIf cfg.enable {
    fonts.fontconfig.enable = true;

    home.packages = [
      pkgs.unstable.nerd-fonts.fira-code
    ];
  };
}
