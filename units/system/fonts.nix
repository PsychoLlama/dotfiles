{ rhizome, units, ... }:

let
  inherit (units.platform) home-manager nixos;
in

rhizome.unit "fonts" {
  unit = rhizome.component [
    (nixos.module (
      { lib, pkgs, ... }:

      {
        fonts.enableDefaultPackages = lib.mkDefault true;

        fonts.packages = [
          pkgs.noto-fonts-color-emoji
        ];

        fonts.fontconfig.defaultFonts.emoji = [ "Noto Color Emoji" ];
      }
    ))

    (home-manager.module (
      { pkgs, ... }:

      {
        fonts.fontconfig.enable = true;

        home.packages = [
          pkgs.unstable.nerd-fonts.fira-code
        ];
      }
    ))
  ];
}
