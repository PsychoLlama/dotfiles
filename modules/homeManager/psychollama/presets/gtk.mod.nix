{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.gtk;
in
{
  options.psychollama.presets.gtk.enable = lib.mkEnableOption "Configure GTK with dark theme";

  config = lib.mkIf cfg.enable {
    gtk = {
      enable = true;
      colorScheme = "dark";
    };

    home.pointerCursor = {
      name = "Adwaita";
      package = pkgs.unstable.adwaita-icon-theme;
      size = 24;
      gtk.enable = true;
    };

    dconf.settings."org/gnome/desktop/interface".color-scheme = "prefer-dark";
  };
}
