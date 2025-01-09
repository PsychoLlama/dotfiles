{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) types;
  cfg = config.programs.nushell.libraries;
in

{
  options.programs.nushell.libraries = {
    enable = lib.mkEnableOption "Manage the library search path";
    path = lib.mkOption {
      type = types.listOf (types.either types.str types.path);
      description = "Libraries visible in the search path";
      default = [ ];
    };

    nu_scripts = {
      enable = lib.mkEnableOption "Add nu_scripts to the library path";
      package = lib.mkPackageOption pkgs "nu_scripts" { };
      completions = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        description = "Completion modules to auto-import";
        default = [ ];
      };
    };
  };

  config.programs.nushell = lib.mkIf cfg.enable {
    libraries.path = lib.optionals cfg.nu_scripts.enable [
      "${cfg.nu_scripts.package}/share/nu_scripts"
    ];

    extraEnv = ''
      ### Add custom libraries to the search path ###
      $env.NU_LIB_DIRS ++= ${lib.hm.nushell.toNushell { } (lib.map toString cfg.path)}
    '';

    extraConfig = lib.mkIf (cfg.nu_scripts.completions != [ ]) ''
      ### 3rd party completions ###
      ${lib.concatMapStringsSep "\n" (cmdName: ''
        use custom-completions/${cmdName}/${cmdName}-completions.nu *
      '') cfg.nu_scripts.completions}
    '';
  };
}
