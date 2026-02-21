{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (config.theme) palette;
  cfg = config.psychollama.presets.programs.ghostty;
  zellij = lib.getExe' config.programs.zellij.package "zellij";
  ghosttyPkg = pkgs.unstable.${if pkgs.stdenv.isDarwin then "ghostty-bin" else "ghostty"};
in

{
  config.programs.ghostty = lib.mkIf cfg.enable {
    package = ghosttyPkg;
    themes.OneDarkPro = {
      background = palette.normal.black;
      foreground = palette.normal.white;
      cursor-color = palette.normal.white;
      cursor-text = palette.normal.black;
      selection-background = palette.bright.black;
      selection-foreground = palette.normal.black;
      palette = [
        "0=${palette.normal.black}"
        "1=${palette.normal.red}"
        "2=${palette.normal.green}"
        "3=${palette.normal.yellow}"
        "4=${palette.normal.blue}"
        "5=${palette.normal.magenta}"
        "6=${palette.normal.cyan}"
        "7=${palette.normal.white}"
        "8=${palette.bright.black}"
        "9=${palette.bright.red}"
        "10=${palette.bright.green}"
        "11=${palette.bright.yellow}"
        "12=${palette.bright.blue}"
        "13=${palette.bright.magenta}"
        "14=${palette.bright.cyan}"
        "15=${palette.bright.white}"
      ];
    };

    settings = {
      theme = "OneDarkPro";
      background-opacity = 0.85;

      font-family = "FiraCode Nerd Font";
      font-style = "Light";
      font-size = 14;
      font-feature = [
        "-calt"
        "-clig"
        "-liga"
      ];

      window-decoration = "none";
      window-padding-x = 0;
      window-padding-y = 0;

      command = toString (
        pkgs.writeShellScript "start-zellij" ''
          exec ${zellij} attach --create
        ''
      );
    };
  };
}
