{ config, lib, ... }:

let cfg = config.presets.exa;

in with lib; {
  options.presets.exa.enable = mkEnableOption "Replace ls with exa";

  config = mkIf cfg.enable {
    programs.exa.enable = true;

    home.shellAliases = {
      ls = "exa";
      l = "exa -la";
    };
  };
}
