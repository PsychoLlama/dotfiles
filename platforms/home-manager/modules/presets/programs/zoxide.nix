{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.presets.programs.zoxide;
in
{
  options.presets.programs.zoxide.enable = mkEnableOption "Use Zoxide to jump around directories";

  config.programs.zoxide = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.zoxide;
  };
}
