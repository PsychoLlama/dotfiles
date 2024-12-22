{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.presets.services.auth-agent;
  socketName = config.services.auth-agent.socket;
in

{
  options.presets.services.auth-agent.enable = lib.mkEnableOption "Cross platform ssh-agent";

  config = lib.mkIf cfg.enable {
    services.auth-agent = {
      enable = true;
      package = pkgs.unstable.openssh;
    };

    home.sessionVariables.SSH_AUTH_SOCK = "\${XDG_RUNTIME_DIR-/tmp}/${socketName}";

    programs.nushell.extraEnv =
      if pkgs.stdenv.isDarwin then
        ''
          $env.SSH_AUTH_SOCK = "/tmp/${socketName}"
        ''
      else
        ''
          $env.SSH_AUTH_SOCK = $"($env.XDG_RUNTIME_DIR)/${socketName}"
        '';
  };
}
