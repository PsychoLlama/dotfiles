{ cfg, lib, ... }:

# A centralized color palette. Pure data: no configuration of its own,
# just options that other modules read off `global`.
#
# TODO: Try some alternatives.
# - https://github.com/Misterio77/nix-colors
# - https://github.com/danth/stylix

let
  inherit (lib) types mkOption;

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
  options = {
    # Data, not an effect: publishing a table configures nothing, and reads
    # through `global` never consult `enable`. There is nothing to opt into,
    # so it defaults on.
    enable = mkOption {
      type = types.bool;
      default = true;
      description = "Whether to publish the theme.";
    };

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
      type = types.attrsOf palette-type;
      default = import ./palettes.nix { inherit lib; };
    };
  };
}
