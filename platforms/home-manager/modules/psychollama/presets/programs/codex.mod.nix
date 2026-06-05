{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.programs.codex;
in

{
  options.psychollama.presets.programs.codex = {
    enable = lib.mkEnableOption "Install the latest version of codex";
  };

  config = lib.mkIf cfg.enable {
    programs.codex = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.unstable.codex;
    };

    programs.git.ignores = [ "**/.codex" ];
  };
}
