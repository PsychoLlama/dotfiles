{ config, lib, pkgs, ... }:

let cfg = config.presets.ncspot;

in with lib; {
  options.presets.ncspot.enable = mkEnableOption "Install and configure ncspot";

  config.programs.ncspot = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.ncspot;
  };
}
