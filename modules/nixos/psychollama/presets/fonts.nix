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
  options.psychollama.presets.fonts = {
    enable = lib.mkEnableOption "Opinionated font configuration";
  };

  config = lib.mkIf cfg.enable {
    fonts.packages = [
      pkgs.noto-fonts-color-emoji
    ];

    fonts.fontconfig.defaultFonts.emoji = [ "Noto Color Emoji" ];
  };
}
