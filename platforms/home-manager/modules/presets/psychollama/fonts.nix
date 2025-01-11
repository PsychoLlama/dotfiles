{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.presets.fonts;
in

{
  options.presets.fonts.enable = lib.mkEnableOption "Use recommended fonts";

  config = lib.mkIf cfg.enable {
    fonts.fontconfig.enable = true;

    home.packages = [
      pkgs.unstable.nerd-fonts.fira-code
    ];
  };
}
