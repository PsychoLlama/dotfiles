{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.presets.programs.ast-grep;
in
{
  options.presets.programs.ast-grep.enable = mkEnableOption "Install and configure ast-grep";

  config.programs.ast-grep = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.ast-grep;
  };
}
