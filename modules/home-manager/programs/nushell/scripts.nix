{ pkgs, config, lib, ... }:

with lib;

let
  cfg = config.programs.nushell.scripts;
  completions = "${cfg.package}/share/nu_scripts/custom-completions";

in {
  options.programs.nushell.scripts = {
    enable = mkEnableOption "Enable 3rd-party scripts";
    package = mkPackageOption pkgs "nu_scripts" { };
    completions = mkOption {
      type = types.listOf types.str;
      description = "Completion modules to auto-import";
      default = [ ];
    };
  };

  config.programs.nushell.extraConfig =
    mkIf (cfg.enable && cfg.completions != [ ]) ''
      ### Completions ###
      ${concatMapStringsSep "\n" (moduleName: ''
        use ${completions}/${moduleName}/${moduleName}-completions.nu *
      '') cfg.completions}
    '';
}
