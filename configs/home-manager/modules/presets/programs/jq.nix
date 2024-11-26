{ config, lib, ... }:

with lib;

let
  cfg = config.presets.programs.jq;
in
{
  options.presets.programs.jq.enable = mkEnableOption "Install and configure jq";

  config.programs.jq = mkIf cfg.enable { enable = true; };
}
