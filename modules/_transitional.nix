# Presets not yet reachable through a profile. Delete an entry as its module
# moves to explicit imports; delete the file when the list empties.

{
  imports = [
    ./programs/bemoji.nix
    ./programs/carapace.nix
    ./programs/chromium.nix
    ./programs/codex
    ./programs/dictation.nix
    ./programs/editor.nix
    ./programs/fuzzel.nix
    ./programs/gh.nix
    ./programs/spotify.nix
    ./programs/swaylock.nix
    ./programs/waybar
    ./programs/wireshark.nix

    ./services/agenix.nix
    ./services/avahi.nix
    ./services/dunst.nix
    ./services/gammastep.nix
    ./services/greetd.nix
    ./services/pipewire.nix
    ./services/podman.nix
    ./services/restic
    ./services/ssh-agent.nix
    ./services/swayidle.nix
    ./services/syncthing.nix
    ./services/tailscale.nix
    ./services/zfs.nix

    ./system/agents
    ./system/gtk.nix
    ./system/sound-theme.nix
  ];
}
