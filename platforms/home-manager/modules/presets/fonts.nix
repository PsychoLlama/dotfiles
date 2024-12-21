{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.presets.fonts;
in
{
  options.presets.fonts.enable = mkEnableOption "Use recommended fonts";

  config = mkIf cfg.enable {
    fonts.fontconfig.enable = true;

    home.packages = with pkgs.unstable; [
      (fira-code.overrideAttrs (attrs: {
        # The default `truetype` name conflicts with nerdfonts.
        postInstall = ''
          mv $out/share/fonts/{truetype,fira-code}
        '';
      }))

      (nerdfonts.override { fonts = [ "FiraCode" ]; })
    ];

    programs.alacritty.settings.font.normal.family = "Fira Code";
  };
}
