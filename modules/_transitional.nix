# Presets not yet reachable through a profile. Delete an entry as its module
# moves to explicit imports; delete the file when the list empties.

{
  imports = [
    ./programs/codex
    ./programs/wireshark.nix

    ./services/agenix.nix
    ./services/avahi.nix
    ./services/greetd.nix
    ./services/pipewire.nix
    ./services/podman.nix
    ./services/restic
    ./services/syncthing.nix
    ./services/tailscale.nix
    ./services/zfs.nix

    ./system/agents
  ];
}
