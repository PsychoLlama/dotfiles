{ config, lib, pkgs, ... }:

let cfg = config.presets.zoxide;

in with lib; {
  options.presets.zoxide.enable =
    mkEnableOption "Use Zoxide to jump around directories";

  config.programs.zoxide = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.zoxide;
  };
}
