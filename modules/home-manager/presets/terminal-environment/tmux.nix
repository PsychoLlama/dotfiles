{ config, lib, pkgs, ... }:

with lib;

let cfg = config.presets.tmux;

in {
  options.presets.tmux.enable =
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
      shell = "${pkgs.zsh}/bin/zsh";
      extraConfig = builtins.readFile ../../../../config/tmux.conf;
    };
  };
}
