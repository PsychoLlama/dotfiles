{ rhizome, units, ... }:

let
  inherit (units.platform) home-manager nixos;
in

[
  (rhizome.component "profiles.full" [

    (nixos.module (
      { lib, ... }:

      let
        inherit (lib) mkDefault;
      in

      {
        services = {
          automatic-timezoned.enable = mkDefault true;
          printing.enable = mkDefault true;
        };

        # Build the apropos/whatis cache so `man -k` works. Carapace's native `man`
        # completer shells out to apropos, which returns nothing without it.
        documentation.man.cache.enable = mkDefault true;
      }
    ))

    (home-manager.module (
      { lib, pkgs, ... }:

      let
        inherit (lib) mkDefault;
      in

      {
        # The Linux man pages aren't a program, so there's no `enable` to reach
        # for. They're what makes section 2 and 3 lookups resolve.
        home.packages = [ pkgs.man-pages ];

        programs = {
          binutils.enable = mkDefault true;
          duf.enable = mkDefault true;
          hexyl.enable = mkDefault true;
          lsof.enable = mkDefault true;
          man.generateCaches = mkDefault true;
          nh.enable = mkDefault true;
          onefetch.enable = mkDefault true;
          parted.enable = mkDefault pkgs.stdenv.isLinux;
          rage.enable = mkDefault true;
          ripgrep.enable = mkDefault true;
          tokei.enable = mkDefault true;
          viu.enable = mkDefault true;
        };
      }
    ))
  ])
  units.profiles.agent-skills
  units.programs.bat
  units.programs.bottom
  units.programs.carapace
  units.programs.claude-code.unit
  units.programs.codex.unit
  units.programs.delta
  units.programs.dictation
  units.programs.direnv
  (home-manager.program "dive")
  (home-manager.program "dix")
  (home-manager.program "doggo")
  units.programs.editor
  units.programs.fd
  (home-manager.program "fzf")
  units.programs.gh
  units.programs.git
  units.programs.glow
  (home-manager.program "jq")
  (home-manager.program "miniserve")
  (home-manager.program "nix-output-monitor")
  units.programs.nushell.unit
  units.programs.nushell.swizzle
  (home-manager.program "python3")
  units.programs.spotify
  units.programs.starship
  units.programs.sway
  (home-manager.program "termshark")
  units.programs.tmux.unit
  (home-manager.program "viddy")
  units.programs.wezterm
  (home-manager.program "whois")
  units.programs.wireshark
  (home-manager.program "zoxide")
  units.services.agenix
  units.services.avahi
  units.services.greetd
  units.services.pipewire
  units.services.podman
  units.services.restic.unit
  units.services.ssh-agent
  units.services.syncthing
  units.services.tailscale
  units.services.zfs
  units.system.fonts
  units.system.observability
  units.system.zram
]
