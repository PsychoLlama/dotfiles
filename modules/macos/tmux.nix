{ lib, config, ... }:

let cfg = config.programs.tmux;

in with lib; {
  options.programs.tmux = {
    historyLimit = mkOption {
      type = types.int;
      default = 2000;
      example = 5000;
      description = ''
        Maximum number of lines held in window history.
      '';
    };

    escapeTime = mkOption {
      type = types.int;
      default = 500;
      example = 0;
      description = ''
        Time in millliseconds for which tmux waits after an escape is input.
      '';
    };

    keyMode = mkOption {
      type = types.str;
      default = "emacs";
      example = "vi";
      description = ''
        Vi or Emacs style shortcuts.
      '';
    };

    customPaneNavigationAndResize = mkOption {
      description = "Stubbed";
      type = types.str;
    };
  };

  config = mkIf cfg.enable {
    programs.tmux.extraConfig = ''
      set -g history-limit ${toString cfg.historyLimit}
      set -g escape-time ${toString cfg.escapeTime}
    '';

    programs.tmux.enableVim = cfg.keyMode == "vi";
  };
}
