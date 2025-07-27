{
  pkgs,
  lib,
  config,
  ...
}:

let
  cfg = config.psychollama.presets.programs.claude-code;
in

{
  options.psychollama.presets.programs.claude-code = {
    enable = lib.mkEnableOption "Opinionated config for Claude Code";
  };

  config.programs.claude-code = lib.mkIf cfg.enable {
    enable = lib.mkDefault true;
    package = lib.mkDefault pkgs.unstable.claude-code;
    settings = {
      includeCoAuthoredBy = false;
    };
  };
}
