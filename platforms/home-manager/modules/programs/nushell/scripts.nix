{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.programs.nushell.scripts;
  completions = lib.map (
    completion: "custom-completions/${completion}/${completion}-completions.nu"
  ) cfg.completions;
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

    modules = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      description = "Modules to auto-import";
      default = [ ];
    };
  };

  config.programs.nushell = lib.mkIf (cfg.enable && cfg.modules != [ ]) {
    extraConfig = ''
      ### Modules from nu_scripts ###
      ${lib.concatMapStringsSep "\n" (moduleName: ''
        use ${cfg.package}/share/nu_scripts/${moduleName} *
      '') (cfg.modules ++ completions)}
    '';
  };
}
