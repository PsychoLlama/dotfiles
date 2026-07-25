{ lib }:

# The palette table. A plain `.nix` helper, not a module: pure data with no
# schema of its own, kept out of `mod.nix` so that file reads as the theme's
# shape rather than its contents.

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
