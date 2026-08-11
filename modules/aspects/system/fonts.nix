{
  flake.modules = {
    nixos.default =
      { lib, pkgs, ... }:

      {
        fonts.enableDefaultPackages = lib.mkDefault true;

        fonts.packages = [
          pkgs.noto-fonts-color-emoji
        ];

        fonts.fontconfig.defaultFonts.emoji = [ "Noto Color Emoji" ];
      };

    homeManager.default =
      { pkgs, ... }:

      {
        fonts.fontconfig.enable = true;

        home.packages = [
          pkgs.unstable.nerd-fonts.fira-code
        ];
      };
  };
}
