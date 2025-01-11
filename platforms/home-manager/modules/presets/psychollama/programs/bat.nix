{ config, lib, ... }:

let
  cfg = config.psychollama.presets.programs.bat;
in

{
  config = lib.mkIf cfg.enable {
    home.shellAliases.cat = "bat";

    programs.bat = {
      config = {
        theme = "TwoDark";
        style = "changes";
      };
    };
  };
}
