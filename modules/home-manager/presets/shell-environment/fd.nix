{ config, lib, pkgs, ... }:

with lib;

let cfg = config.presets.fd;

in {
  options.presets.fd.enable = mkEnableOption "Whether to enable fd-find";

  config = mkIf cfg.enable {
    programs.fd = {
      enable = true;
      package = pkgs.unstable.fd;
    };

    programs.fzf.defaultCommand = "fd --type f";
  };
}
