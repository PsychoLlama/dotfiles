{ config, lib, ... }:

with lib;

let cfg = config.presets.jq;

in {
  options.presets.jq.enable = mkEnableOption "Install and configure jq";

  config.programs.jq = mkIf cfg.enable { enable = true; };
}
