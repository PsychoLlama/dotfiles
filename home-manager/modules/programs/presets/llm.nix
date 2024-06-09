{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.presets.llm;
in

{
  options.programs.presets.llm = {
    enable = lib.mkEnableOption "Interact with LLMs through the command line";
  };

  config = lib.mkIf cfg.enable {
    programs.llm = {
      enable = true;
      package = pkgs.unstable.llm;
    };
  };
}
