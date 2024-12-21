{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.presets.programs.fd;
in
{
  options.presets.programs.fd.enable = mkEnableOption "Whether to enable fd-find";

  config = mkIf cfg.enable {
    programs.fd = {
      enable = true;
      package = pkgs.unstable.fd;
    };

    programs.fzf.defaultCommand = "fd --type f";
  };
}
