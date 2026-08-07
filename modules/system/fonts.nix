{ lib, ... }:

let
  option.options.psychollama.presets.fonts.enable =
    lib.mkEnableOption "Opinionated font configuration";
in

{
  flake.modules = {
    nixos.default =
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
        imports = [ option ];

        config = lib.mkIf cfg.enable {
          fonts.enableDefaultPackages = lib.mkDefault true;

          fonts.packages = [
            pkgs.noto-fonts-color-emoji
          ];

          fonts.fontconfig.defaultFonts.emoji = [ "Noto Color Emoji" ];
        };
      };

    homeManager.default =
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
        imports = [ option ];

        config = lib.mkIf cfg.enable {
          fonts.fontconfig.enable = true;

          home.packages = [
            pkgs.unstable.nerd-fonts.fira-code
          ];
        };
      };
  };
}
