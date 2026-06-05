{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.services.ssh-agent;
in

{
  options.psychollama.presets.services.ssh-agent = {
    enable = lib.mkEnableOption "OpenSSH key agent";
  };

  config.services.ssh-agent = lib.mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.openssh;
  };
}
