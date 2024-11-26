{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.presets.programs.llm;
in

{
  options.presets.programs.llm = {
    enable = lib.mkEnableOption "Interact with LLMs through the command line";
  };

  config = lib.mkIf cfg.enable {
    programs.llm = {
      enable = true;
      package = pkgs.unstable.llm;
    };
  };
}
