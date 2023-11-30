{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.presets.auth-agent;
  socketName = config.services.auth-agent.socket;

in {
  options.presets.auth-agent.enable = mkEnableOption "Cross platform ssh-agent";

  config = mkIf cfg.enable {
    services.auth-agent.enable = true;

    home.sessionVariables.SSH_AUTH_SOCK =
      "\${XDG_RUNTIME_DIR-/tmp}/${socketName}";

    programs.nushell.extraEnv = if pkgs.stdenv.isDarwin then ''
      $env.SSH_AUTH_SOCK = "/tmp/${socketName}"
    '' else ''
      $env.SSH_AUTH_SOCK = $"($env.XDG_RUNTIME_DIR)/${socketName}"
    '';
  };
}
