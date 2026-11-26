{ rhizome, units, ... }:

let
  inherit (units.platform) home-manager;
in

rhizome.unit "sound-theme" (sound-theme: {
  /**
    Plays a sound from the theme by name: `play-sound message-new-instant`.

    # Type

    ```
    player :: Pkgs -> Package
    ```
  */
  player =
    pkgs:
    pkgs.writeShellApplication {
      name = "play-sound";
      runtimeInputs = [
        pkgs.pipewire
        pkgs.procps
      ];

      text = ''
        pkill -x pw-play || true
        pw-play "${pkgs.unstable.kdePackages.ocean-sound-theme}/share/sounds/ocean/stereo/$1.oga"
      '';
    };

  unit = rhizome.component [
    (home-manager.module (
      { pkgs, ... }:

      {
        home.packages = [ (sound-theme.player pkgs) ];

        dconf.settings."org/gnome/desktop/sound" = {
          theme-name = "ocean";
          event-sounds = true;
        };
      }
    ))
  ];
})
