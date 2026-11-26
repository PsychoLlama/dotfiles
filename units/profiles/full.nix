{ rhizome, units, ... }:

let
  inherit (units.platform) home-manager nixos;
in

rhizome.unit "profiles.full" (full: {
  main = rhizome.component [

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
  ];

  unit = [
    full.main
    units.agents.unit
    units.profiles.agent-skills.unit
    units.programs.bat.unit
    units.programs.bottom.unit
    units.programs.carapace.unit
    units.programs.claude-code.unit
    units.programs.codex.unit
    units.programs.delta.unit
    units.programs.dictation.unit
    units.programs.direnv.unit
    (home-manager.program "dive")
    (home-manager.program "dix")
    (home-manager.program "doggo")
    units.editor.unit
    units.editor.profiles.full.unit
    units.programs.fd.unit
    (home-manager.program "fzf")
    units.programs.gh.unit
    units.programs.git.unit
    units.programs.glow.unit
    (home-manager.program "jq")
    (home-manager.program "miniserve")
    (home-manager.program "nix-output-monitor")
    units.programs.nushell.unit
    units.programs.nushell.swizzle.unit
    (home-manager.program "python3")
    units.programs.spotify.unit
    units.programs.starship.unit
    units.programs.sway.unit
    (home-manager.program "termshark")
    units.programs.tmux.unit
    (home-manager.program "viddy")
    units.programs.wezterm.unit
    (home-manager.program "whois")
    units.programs.wireshark.unit
    (home-manager.program "zoxide")
    units.services.agenix.unit
    units.services.avahi.unit
    units.services.greetd.unit
    units.services.pipewire.unit
    units.services.podman.unit
    units.services.restic.unit
    units.services.ssh-agent.unit
    units.services.syncthing.unit
    units.services.tailscale.unit
    units.services.zfs.unit
    units.system.fonts.unit
    units.system.observability.unit
    units.system.zram.unit
  ];
})
