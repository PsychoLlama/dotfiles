{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.programs.gh;
in

{
  options.psychollama.presets.programs.gh = {
    enable = lib.mkEnableOption "Opinionated config for GitHub CLI";
  };

  config.programs.gh = lib.mkIf cfg.enable {
    enable = lib.mkDefault true;
    package = pkgs.unstable.gh;

    settings = {
      git_protocol = "ssh";
    };
  };
}
