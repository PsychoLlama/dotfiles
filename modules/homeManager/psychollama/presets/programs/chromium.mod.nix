{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.programs.chromium;
in

{
  options.psychollama.presets.programs.chromium.enable =
    lib.mkEnableOption "Install ungoogled-chromium";

  config.programs.chromium = lib.mkIf cfg.enable {
    enable = lib.mkDefault true;
    package = lib.mkDefault pkgs.unstable.chromium;
  };
}
