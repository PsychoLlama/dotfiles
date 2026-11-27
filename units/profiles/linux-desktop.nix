{
  units,
  self,
  define,
  ...
}:

let
  inherit (units.platform) home-manager;
in

{
  main = define.unit {
    config = [

      (home-manager.module ({ pkgs, ... }: { home.packages = [ pkgs.signal-desktop ]; }))
    ];
  };

  unit = define.group {
    config = [
      self.main
      (home-manager.lib.package "acpi")
      units.programs.bemoji.unit
      (home-manager.lib.package "brightnessctl")
      units.programs.chromium.unit
      units.programs.firefox.unit
      units.programs.fuzzel.unit
      (home-manager.lib.package "grim")
      (home-manager.lib.package "pamixer")
      (home-manager.lib.package "parted")
      (home-manager.lib.package "playerctl")
      (home-manager.lib.package "slurp")
      units.programs.sway.unit
      units.programs.swaylock.unit
      units.programs.waybar.unit
      (home-manager.lib.package "wireplumber")
      (home-manager.lib.package "wl-clipboard")
      units.services.dunst.unit
      units.services.gammastep.unit
      units.services.network-manager.unit
      units.services.swaybg.unit
      units.services.swayidle.unit
      units.system.fonts.unit
      units.system.gtk.unit
      units.system.sound-theme.unit
    ];
  };
}
