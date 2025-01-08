{
  pkgs,
  config,
  lib,
  ...
}:

# TODO: Merge this with `nu_scripts` config.

let
  cfg = config.programs.nushell.scripts;
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

  config.programs.nushell = lib.mkIf (cfg.enable && cfg.completions != [ ]) {
    extraConfig = ''
      ### 3rd party completions ###
      ${lib.concatMapStringsSep "\n" (cmdName: ''
        use custom-completions/${cmdName}/${cmdName}-completions.nu *
      '') cfg.completions}
    '';
  };
}
