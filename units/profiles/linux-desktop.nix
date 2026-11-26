{ rhizome, units, ... }:

let
  inherit (units.platform) home-manager;
in

rhizome.unit "profiles.linux-desktop" (linux-desktop: {
  main = rhizome.component [

    (home-manager.module (
      { lib, ... }:

      {
        programs.signal-desktop.enable = lib.mkDefault true;
      }
    ))
  ];

  unit = [
    linux-desktop.main
    (home-manager.program "acpi")
    units.programs.bemoji.unit
    (home-manager.program "brightnessctl")
    units.programs.chromium.unit
    units.programs.firefox.unit
    units.programs.fuzzel.unit
    (home-manager.program "grim")
    (home-manager.program "pamixer")
    (home-manager.program "parted")
    (home-manager.program "playerctl")
    (home-manager.program "slurp")
    units.programs.sway.unit
    units.programs.swaylock.unit
    units.programs.waybar.unit
    (home-manager.program "wireplumber")
    (home-manager.program "wl-clipboard")
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
