{ lib, config, pkgs, ... }:

with lib;

let
  cfg = config.plugins.coc-nvim;
  jsonFormat = pkgs.formats.json { };
  configDir = pkgs.linkFarm "coc-settings" {
    "coc-settings.json" = jsonFormat.generate "coc-settings.json" cfg.settings;
  };

in {
  options.plugins.coc-nvim.settings = mkOption {
    type = jsonFormat.type;
    default = { };
    description = ''
      Configuration for coc.nvim.
    '';
  };

  config.extraConfig = mkIf cfg.enable ''
    let g:coc_config_home = "${configDir}"
  '';
}
