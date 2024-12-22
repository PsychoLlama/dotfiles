{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.presets.programs.zoxide;
in

{
  options.presets.programs.zoxide.enable = lib.mkEnableOption "Use Zoxide to jump around directories";

  config.programs.zoxide = lib.mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.zoxide;
  };
}
