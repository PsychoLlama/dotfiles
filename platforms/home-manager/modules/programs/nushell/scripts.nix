{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.programs.nushell.scripts;
  completions = "${cfg.package}/share/nu_scripts/custom-completions";
in

{
  options.programs.nushell.scripts = {
    enable = lib.mkEnableOption "Enable 3rd-party scripts";
    package = lib.mkPackageOption pkgs "nu_scripts" { };
    completions = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      description = "Completion modules to auto-import";
      default = [ ];
    };
  };

  config.programs.nushell.extraConfig = lib.mkIf (cfg.enable && cfg.completions != [ ]) ''
    ### Completions ###
    ${lib.concatMapStringsSep "\n" (moduleName: ''
      use ${completions}/${moduleName}/${moduleName}-completions.nu *
    '') cfg.completions}
  '';
}
