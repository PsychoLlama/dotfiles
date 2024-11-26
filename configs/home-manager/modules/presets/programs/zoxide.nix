{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.programs.presets.zoxide;
in
{
  options.programs.presets.zoxide.enable = mkEnableOption "Use Zoxide to jump around directories";

  config.programs.zoxide = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.zoxide;
  };
}
