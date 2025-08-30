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

  # Only enabled on Linux. Darwin/launchd are miserable to debug and cause horrible permission issues.
  # It's infinitely easier to run it in a background tmux session.
  config = lib.mkIf (cfg.enable && pkgs.stdenv.isLinux) {
    services.emacs = {
      enable = true;
      client.enable = lib.mkDefault true;
      startWithUserSession = lib.mkDefault true;
      defaultEditor = false; # Managed by program preset.
    };

    systemd.user.services.emacs.Service = {
      # Override shell to inherit standard nushell environment (i.e. dynamic env vars).
      ExecStart = lib.mkForce "${nu}/bin/nu --login -c 'emacs --fg-daemon'";
      Type = lib.mkForce "exec";
    };
  };
}
