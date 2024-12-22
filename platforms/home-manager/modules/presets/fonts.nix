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
      (pkgs.unstable.fira-code.overrideAttrs (attrs: {
        # The default `truetype` name conflicts with nerdfonts.
        postInstall = ''
          mv $out/share/fonts/{truetype,fira-code}
        '';
      }))

      (pkgs.unstable.nerdfonts.override { fonts = [ "FiraCode" ]; })
    ];

    programs.alacritty.settings.font.normal.family = "Fira Code";
  };
}
