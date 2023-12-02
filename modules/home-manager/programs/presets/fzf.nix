{ config, lib, ... }:

with lib;

let cfg = config.programs.presets.fzf;

in {
  options.programs.presets.fzf.enable = mkEnableOption "Whether to use fzf";
  config.programs.fzf.enable = cfg.enable;
}
