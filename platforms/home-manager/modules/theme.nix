{ config, lib, ... }:

# Defines a centralized color palette that I can use in other configs.
# There are nix modules I can install that do something similar, but I'm not
# ready to try those yet.
#
# TODO: Try some alternatives.
# - https://github.com/Misterio77/nix-colors
# - https://github.com/danth/stylix

let
  inherit (lib) types mkOption mkDefault;
  cfg = config.theme;

  colors-type = types.submodule {
    options = {
      black = mkOption { type = types.str; };
      red = mkOption { type = types.str; };
      green = mkOption { type = types.str; };
      yellow = mkOption { type = types.str; };
      blue = mkOption { type = types.str; };
      magenta = mkOption { type = types.str; };
      cyan = mkOption { type = types.str; };
      white = mkOption { type = types.str; };
    };
  };

  palette-type = types.submodule {
    options = {
      normal = mkOption {
        description = "Normal half of a base16 color palette.";
        type = colors-type;
      };

      bright = mkOption {
        description = "Bright half of a base16 color palette.";
        type = colors-type;
      };
    };
  };
in
{
  options.theme = {
    name = mkOption {
      type = types.enum (lib.attrNames cfg.palettes);
      default = "one-dark";
      description = "Name of the color palette to use.";
    };

    palette = mkOption {
      type = palette-type;
      readOnly = true;
      default = cfg.palettes.${cfg.name};
      description = "The selected color palette.";
    };

    palettes = mkOption {
      description = "All color palettes";
      default = { };
      type = types.attrsOf palette-type;
    };
  };

  config.theme = {
    palettes.one-dark = {
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
  };
}
