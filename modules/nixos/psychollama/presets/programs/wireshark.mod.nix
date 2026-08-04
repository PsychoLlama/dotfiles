{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.programs.wireshark;
in

{
  options.psychollama.presets.programs.wireshark = {
    enable = lib.mkEnableOption "Install and configure pkgs.wireshark";
  };

  config.programs.wireshark = lib.mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.wireshark;
  };
}
