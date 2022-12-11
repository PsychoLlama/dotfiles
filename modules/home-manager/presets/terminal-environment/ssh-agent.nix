{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.presets.ssh-agent;
  socketName = config.services.ssh-agent.socket;

in {
  options.presets.ssh-agent.enable = mkEnableOption "ssh-agent";

  config = mkIf cfg.enable {
    services.ssh-agent.enable = true;

    home.sessionVariables.SSH_AUTH_SOCK =
      "\${XDG_RUNTIME_DIR-/tmp}/${socketName}";

    programs.nushell.initExtra = if pkgs.stdenv.isDarwin then ''
      let-env SSH_AUTH_SOCK = "/tmp/${socketName}"
    '' else ''
      let-env SSH_AUTH_SOCK = $"($env.XDG_RUNTIME_DIR)/${socketName}"
    '';
  };
}
