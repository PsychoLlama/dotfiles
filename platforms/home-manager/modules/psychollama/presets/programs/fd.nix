{ config, lib, ... }:

let
  cfg = config.psychollama.presets.programs.fd;
in

{
  config = lib.mkIf cfg.enable {
    programs.fzf.defaultCommand = "fd --type f";
  };
}
