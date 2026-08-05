{
  dotfiles.fonts = {
    nixos =
      { pkgs, ... }:

      {
        fonts = {
          enableDefaultPackages = true;
          packages = [ pkgs.noto-fonts-color-emoji ];
          fontconfig.defaultFonts.emoji = [ "Noto Color Emoji" ];
        };
      };

    homeManager =
      { pkgs, ... }:

      {
        fonts.fontconfig.enable = true;
        home.packages = [ pkgs.unstable.nerd-fonts.fira-code ];
      };
  };
}
