{ config, lib, pkgs, ... }:

let cfg = config.presets.dive;

in with lib; {
  options.presets.dive.enable = mkEnableOption "Install and configure dive";

  config.programs.dive = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.dive;
  };
}
