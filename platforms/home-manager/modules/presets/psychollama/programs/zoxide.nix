{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.programs.zoxide;
in

{
  options.psychollama.presets.programs.zoxide.enable = lib.mkEnableOption "Use Zoxide to jump around directories";

  config.programs.zoxide = lib.mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.zoxide;
  };
}
