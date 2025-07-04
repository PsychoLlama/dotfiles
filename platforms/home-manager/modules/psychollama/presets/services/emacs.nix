{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.services.emacs;
  nu = config.programs.nushell.package;
in

{
  options.psychollama.presets.services.emacs = {
    enable = lib.mkEnableOption "Start Emacs as a daemon";
  };

  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      {
        services.emacs = {
          enable = true;
          client.enable = lib.mkDefault true;
          startWithUserSession = lib.mkDefault true;
          defaultEditor = lib.mkDefault true;
        };
      }

      (lib.mkIf pkgs.stdenv.isLinux {
        systemd.user.services.emacs.Service = {
          # Override shell to inherit standard nushell environment (i.e. dynamic env vars).
          ExecStart = lib.mkForce "${nu}/bin/nu --login -c 'emacs --fg-daemon'";
          Type = lib.mkForce "exec";
        };
      })
    ]
  );
}
