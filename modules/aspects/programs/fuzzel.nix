{ config, ... }:

# Bound out here because the module below shadows `config` with its own.
let
  inherit (config.theme) palette;

  textColor = palette.normal.white;
  accentColor = palette.normal.blue;
in

{
  imports = [ ../../system/theme.nix ];

  flake.modules.homeManager.default =
    {
      lib,
      pkgs,
      ...
    }:

    let
      rgba = hex: alpha: "${lib.substring 1 (-1) hex}${lib.toHexString (builtins.ceil (alpha * 255))}";
      opaque = hex: rgba hex 1.0;
    in

    {
      programs.fuzzel = {
        enable = true;
        package = pkgs.unstable.fuzzel;
        settings = {
          main = {
            horizontal-pad = 16;
            vertical-pad = 8;
            inner-pad = 8;
            match-counter = "yes";
          };

          border = {
            radius = 4;
            width = 0;
            selection-radius = 2;
          };

          colors = {
            background = rgba palette.normal.black 0.8;
            text = opaque textColor;
            prompt = opaque palette.bright.black;
            placeholder = opaque textColor;
            input = opaque textColor;
            match = opaque accentColor;
            selection = rgba palette.bright.black 0.8;
            selection-text = opaque palette.normal.white;
            selection-match = opaque accentColor;
            counter = opaque palette.bright.black;
            border = opaque palette.normal.black;
          };
        };
      };
    };
}
