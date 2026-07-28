{ pkgs, ... }:

# Historically two files: NixOS owned the system-wide emoji fallback and
# home-manager owned the user's font packages. Same concept, same name,
# two option namespaces that could never be enabled together.
{
  modules.nixos.fonts = {
    packages = [ pkgs.noto-fonts-color-emoji ];
    fontconfig.defaultFonts.emoji = [ "Noto Color Emoji" ];
  };

  modules.home-manager = {
    fonts.fontconfig.enable = true;

    home.packages = [ pkgs.unstable.nerd-fonts.fira-code ];
  };
}
