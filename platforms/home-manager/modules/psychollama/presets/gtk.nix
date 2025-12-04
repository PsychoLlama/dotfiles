{ config, lib, ... }:

let
  cfg = config.psychollama.presets.gtk;
in
{
  options.psychollama.presets.gtk.enable = lib.mkEnableOption "Configure GTK with dark theme";

  config.gtk = lib.mkIf cfg.enable {
    enable = true;
    colorScheme = "dark";
  };
}
