{ config, lib, ... }:

with lib;

let cfg = config.presets.fzf;

in {
  options.presets.fzf.enable = mkEnableOption "Whether to use fzf";
  config.programs.fzf.enable = cfg.enable;
}
