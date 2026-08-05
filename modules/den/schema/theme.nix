{ lib, ... }:

let
  inherit (lib) mkOption types;

  # Half of a base16 palette. Keys name the colors, values are the defaults.
  half =
    description: defaults:
    mkOption {
      inherit description;
      default = { };
      type = types.submodule {
        options = lib.mapAttrs (
          _: default:
          mkOption {
            type = types.str;
            inherit default;
          }
        ) defaults;
      };
    };
in

{
  # Entity data, not platform config: aspects read it as `host.theme`, which
  # removes the need to bridge it from `osConfig` into home-manager.
  den.schema.host.options.theme.palette = {
    normal = half "Normal half of the color palette." {
      black = "#1e1e1e";
      red = "#e06c75";
      green = "#98c379";
      yellow = "#e5c07b";
      blue = "#61afef";
      magenta = "#c678dd";
      cyan = "#56b6c2";
      white = "#abb2bf";
    };

    bright = half "Bright half of the color palette." {
      black = "#3e4451";
      red = "#ff7a85";
      green = "#a8d389";
      yellow = "#f0d08b";
      blue = "#71bfff";
      magenta = "#d688ed";
      cyan = "#66c6d2";
      white = "#ffffff";
    };
  };
}
