{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.nushell.nu_scripts;
in

{
  options.programs.nushell.nu_scripts = {
    enable = lib.mkEnableOption "Add nu_scripts to the library path";
    package = lib.mkPackageOption pkgs "nu_scripts" { };
  };

  config.programs.nushell = lib.mkIf cfg.enable {
    extraEnv = ''
      ### Add `nu_scripts` to the library path ###
      $env.NU_LIB_DIRS ++= ${
        lib.hm.nushell.toNushell { } [
          "${cfg.package}/share/nu_scripts"
        ]
      }
    '';
  };
}
