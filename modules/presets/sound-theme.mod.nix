{ lib, pkgs, ... }:

let
  oceanSounds = "${pkgs.unstable.kdePackages.ocean-sound-theme}/share/sounds/ocean/stereo";

  playSound = pkgs.writeShellApplication {
    name = "play-sound";
    runtimeInputs = [
      pkgs.pipewire
      pkgs.procps
    ];

    text = ''
      pkill -x pw-play || true
      pw-play "${oceanSounds}/$1.oga"
    '';
  };
in

{
  options = {
    play = lib.mkOption {
      type = lib.types.str;
      readOnly = true;
      default = lib.getExe' playSound "play-sound";
      description = "Path to the play-sound executable.";
    };
  };

  platforms.home-manager = {
    home.packages = [ playSound ];

    dconf.settings."org/gnome/desktop/sound" = {
      theme-name = "ocean";
      event-sounds = true;
    };
  };
}
