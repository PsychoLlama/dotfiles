{ config, lib, ... }:

let
  cfg = config.presets.programs.fd;
in

{
  config = lib.mkIf cfg.enable {
    programs.fzf.defaultCommand = "fd --type f";
  };
}
