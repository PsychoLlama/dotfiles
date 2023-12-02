{ config, lib, pkgs, ... }:

with lib;

let cfg = config.programs.presets.tmux;

in {
  options.programs.presets.tmux.enable =
    mkEnableOption "Use an opinionated tmux configuration";

  config = mkIf cfg.enable {
    home.shellAliases.t = "tmux";

    programs.tmux = {
      enable = true;
      package = pkgs.unstable.tmux;
      customPaneNavigationAndResize = true;
      escapeTime = 0;
      historyLimit = 100000;
      keyMode = "vi";
      shell = "${pkgs.unstable.nushell}/bin/nu";
      extraConfig = builtins.readFile ../../../../config/tmux.conf;
    };
  };
}
