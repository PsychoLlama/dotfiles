{ lib }:

# The palette table. A plain `.nix` helper, not a module: the theme
# meta-module owns it, and `platforms/universal` imports it directly so
# evals with no meta layer (the editor) still see a valid palette set.

let
  inherit (lib) mkDefault;
in

{
  one-dark = {
    normal = {
      black = mkDefault "#1e1e1e";
      red = mkDefault "#e06c75";
      green = mkDefault "#98c379";
      yellow = mkDefault "#e5c07b";
      blue = mkDefault "#61afef";
      magenta = mkDefault "#c678dd";
      cyan = mkDefault "#56b6c2";
      white = mkDefault "#abb2bf";
    };

    bright = {
      black = mkDefault "#3e4451";
      red = mkDefault "#ff7a85";
      green = mkDefault "#a8d389";
      yellow = mkDefault "#f0d08b";
      blue = mkDefault "#71bfff";
      magenta = mkDefault "#d688ed";
      cyan = mkDefault "#66c6d2";
      white = mkDefault "#ffffff";
    };
  };
}
