{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.programs.direnv;
in

{
  config.programs.direnv = lib.mkIf cfg.enable {
    enableNushellIntegration = false;
    nix-direnv.enable = true;

    config = {
      whitelist.prefix = [ "${config.home.homeDirectory}/projects/psychollama" ];
      global.hide_env_diff = true;
    };
  };

  # Forked from home-manager. Nushell removed the `let-env` command.
  config.programs.nushell.extraConfig = lib.mkIf cfg.enable (
    lib.mkAfter ''
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
    ''
  );

  config.programs.git = lib.mkIf cfg.enable {
    ignores = [ ".direnv/" ];
  };
}
