{ config, lib, ... }:

let
  cfg = config.psychollama.presets.programs.delta;
in

{
  config.programs.delta = lib.mkIf cfg.enable {
    enableGitIntegration = true;

    options = {
      dark = true;
      syntax-theme = "OneHalfDark";
    };
  };
}
