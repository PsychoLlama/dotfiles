{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) types;
  cfg = config.services.swaybg;
in

{
  options.services.swaybg = {
    enable = lib.mkEnableOption "Enable the swaybg wallpaper daemon";
    package = lib.mkPackageOption pkgs "swaybg" { };

    output = lib.mkOption {
      type = types.str;
      default = "*";
      description = "Sway output to set the background on.";
    };

    mode = lib.mkOption {
      type = types.enum [
        "fill"
        "stretch"
        "center"
        "tile"
        "fit"
        "solid_color"
      ];
      default = "fill";
      description = "How to fit the background image.";
    };

    color = lib.mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "Set a solid background color.";
      example = "#663399";
    };

    image = lib.mkOption {
      type = types.nullOr (
        types.oneOf [
          types.path
          types.str
        ]
      );
      default = null;
      description = "Path to a background image relative to $HOME.";
      example = "./Pictures/wallpaper.png";
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.user.services.swaybg = {
      Install.WantedBy = [ "sway-session.target" ];

      Unit = {
        Description = "Sway wallpaper and background service";
        PartOf = [ "sway-session.target" ];
        Documentation = "man:swaybg(1)";
      };

      Service = {
        Type = "simple";
        ExecStart = "${cfg.package}/bin/swaybg ${
          lib.concatStringsSep " " (
            lib.cli.toGNUCommandLine { } {
              image = cfg.image;
              output = cfg.output;
              mode = cfg.mode;
              color = cfg.color;
            }
          )
        }";
      };
    };
  };
}
