{ config, lib, pkgs, ... }:

with lib;

let cfg = config.programs.presets.wezterm;

in {
  options.programs.presets.wezterm.enable =
    mkEnableOption "Install and configure wezterm";

  config.programs.wezterm = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.wezterm;
    colorSchemes = { };
  };
}
