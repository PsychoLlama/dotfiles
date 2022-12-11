{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.presets.ssh-agent;
  sockPath = config.services.ssh-agent.socket;

in {
  options.presets.ssh-agent.enable = mkEnableOption "ssh-agent";

  config = mkIf cfg.enable {
    services.ssh-agent.enable = true;

    home.sessionVariables.SSH_AUTH_SOCK = "\${XDG_RUNTIME_DIR}/${sockPath}";
    programs.nushell.initExtra = ''
      let-env SSH_AUTH_SOCK = $"($env.XDG_RUNTIME_DIR)/${sockPath}"
    '';
  };
}
