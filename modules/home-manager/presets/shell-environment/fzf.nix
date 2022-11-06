{ config, lib, ... }:

let cfg = config.presets.fzf;

in with lib; {
  options.presets.fzf.enable = mkEnableOption "Whether to use fzf";
  config.programs.fzf.enable = cfg.enable;
}
