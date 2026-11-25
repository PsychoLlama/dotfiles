{ rhizome, units, ... }:

let
  inherit (units.platform) home-manager;
in

[
  (rhizome.component "profiles.linux-desktop" [

    (home-manager.module (
      { lib, ... }:

      {
        programs.signal-desktop.enable = lib.mkDefault true;
      }
    ))
  ])
  (home-manager.program "acpi")
  units.programs.bemoji
  (home-manager.program "brightnessctl")
  units.programs.chromium
  units.programs.firefox
  units.programs.fuzzel
  (home-manager.program "grim")
  (home-manager.program "pamixer")
  (home-manager.program "parted")
  (home-manager.program "playerctl")
  (home-manager.program "slurp")
  units.programs.sway
  units.programs.swaylock
  units.programs.waybar.unit
  (home-manager.program "wireplumber")
  (home-manager.program "wl-clipboard")
  units.services.dunst
  units.services.gammastep
  units.services.network-manager
  units.services.swaybg
  units.services.swayidle
  units.system.fonts
  units.system.gtk
  units.system.sound-theme
]
