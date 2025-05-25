{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.programs.llm;
in

{
  options.psychollama.presets.programs.llm = {
    enable = lib.mkEnableOption "Install the latest version of llm";
  };

  config.programs.llm = lib.mkIf cfg.enable {
    enable = lib.mkDefault true;
    package = lib.mkDefault pkgs.unstable.llm;
  };
}
