{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.programs.presets.ast-grep;
in
{
  options.programs.presets.ast-grep.enable = mkEnableOption "Install and configure ast-grep";

  config.programs.ast-grep = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.ast-grep;
  };
}
