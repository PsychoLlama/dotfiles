{ config, lib, pkgs, ... }:

with lib;

let cfg = config.presets.direnv;

in {
  options.presets.direnv.enable = mkEnableOption "Install and configure direnv";
  config.programs.direnv = mkIf cfg.enable {
    enable = true;
    enableNushellIntegration = false;
    nix-direnv.enable = true;
    config.whitelist.prefix =
      [ "${config.home.homeDirectory}/projects/psychollama" ];
  };

  # Forked from home-manager. Nushell removed the `let-env` command.
  config.programs.nushell.extraConfig = mkIf cfg.enable (mkAfter ''
    $env.config = ($env | default {} config).config
    $env.config = ($env.config | default {} hooks)
    $env.config = ($env.config | update hooks ($env.config.hooks | default [] pre_prompt))
    $env.config = ($env.config | update hooks.pre_prompt ($env.config.hooks.pre_prompt | append {
      code: "
        let direnv = (${pkgs.direnv}/bin/direnv export json | from json)
        let direnv = if $direnv == null { {} } else { $direnv }
        $direnv | load-env
        "
    }))
  '');
}
