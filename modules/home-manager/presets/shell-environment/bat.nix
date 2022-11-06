{ config, lib, ... }:

let cfg = config.presets.bat;

in with lib; {
  options.presets.bat.enable = mkEnableOption "Replace cat with bat";

  config = mkIf cfg.enable {
    home.shellAliases.cat = "bat";

    programs.bat = {
      enable = true;
      config = {
        theme = "TwoDark";
        style = "changes";
      };
    };
  };
}
