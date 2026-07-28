{
  cfg,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) types;

  optionFormat = name: {
    option = if builtins.stringLength name > 1 then "--${name}" else "-${name}";
    sep = null;
    explicitBool = false;
  };
in

{
  options = {
    package = lib.mkPackageOption pkgs.unstable "swaybg" { };

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
      default = "attic/images/wallpapers/current";
      description = "Path to a background image relative to $HOME.";
      example = "./Pictures/wallpaper.png";
    };
  };

  modules.home-manager.systemd.user.services.swaybg = {
    Install.WantedBy = [ "sway-session.target" ];

    Unit = {
      Description = "Sway wallpaper and background service";
      PartOf = [ "sway-session.target" ];
      Documentation = "man:swaybg(1)";
    };

    Service = {
      Type = "simple";
      ExecStart = "${lib.getExe cfg.package} ${
        lib.concatStringsSep " " (
          lib.cli.toCommandLine optionFormat {
            inherit (cfg)
              image
              output
              mode
              color
              ;
          }
        )
      }";
    };
  };
}
