{ rhizome, units, ... }:

let
  inherit (units.platform) home-manager;
in

rhizome.unit "profiles.linux-desktop" (linux-desktop: {
  main = rhizome.component [

    (home-manager.module ({ pkgs, ... }: { home.packages = [ pkgs.signal-desktop ]; }))
  ];

  unit = [
    linux-desktop.main
    (home-manager.package "acpi")
    units.programs.bemoji.unit
    (home-manager.package "brightnessctl")
    units.programs.chromium.unit
    units.programs.firefox.unit
    units.programs.fuzzel.unit
    (home-manager.package "grim")
    (home-manager.package "pamixer")
    (home-manager.package "parted")
    (home-manager.package "playerctl")
    (home-manager.package "slurp")
    units.programs.sway.unit
    units.programs.swaylock.unit
    units.programs.waybar.unit
    (home-manager.package "wireplumber")
    (home-manager.package "wl-clipboard")
    units.services.dunst.unit
    units.services.gammastep.unit
    units.services.network-manager.unit
    units.services.swaybg.unit
    units.services.swayidle.unit
    units.system.fonts.unit
    units.system.gtk.unit
    units.system.sound-theme.unit
  ];
})
