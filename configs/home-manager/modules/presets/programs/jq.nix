{ config, lib, ... }:

with lib;

let
  cfg = config.programs.presets.jq;
in
{
  options.programs.presets.jq.enable = mkEnableOption "Install and configure jq";

  config.programs.jq = mkIf cfg.enable { enable = true; };
}
