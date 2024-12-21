{ config, lib, ... }:

with lib;

let
  cfg = config.presets.programs.fzf;
in
{
  options.presets.programs.fzf.enable = mkEnableOption "Whether to use fzf";
  config.programs.fzf.enable = cfg.enable;
}
