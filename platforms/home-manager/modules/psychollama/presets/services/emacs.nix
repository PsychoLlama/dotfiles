{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.services.emacs;
in

{
  options.psychollama.presets.services.emacs = {
    enable = lib.mkEnableOption "Start Emacs as a daemon";
  };

  config.services.emacs = lib.mkIf cfg.enable {
    enable = true;
    client.enable = lib.mkDefault true;
    startWithUserSession = lib.mkDefault true;
  };
}
