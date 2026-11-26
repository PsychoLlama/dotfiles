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
        home.packages = [
          pkgs.man-pages

          pkgs.binutils
          pkgs.duf
          pkgs.hexyl
          pkgs.lsof
          pkgs.onefetch
          pkgs.rage
          pkgs.tokei
        ];

        programs = {
          man.generateCaches = mkDefault true;
          nh.enable = mkDefault true;
          ripgrep.enable = mkDefault true;
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
    (home-manager.package "dive")
    (home-manager.package "dix")
    (home-manager.package "doggo")
    units.editor.unit
    units.editor.profiles.full.unit
    units.programs.fd.unit
    (home-manager.program "fzf")
    units.programs.gh.unit
    units.programs.git.unit
    units.programs.glow.unit
    (home-manager.program "jq")
    (home-manager.package "miniserve")
    (home-manager.package "nix-output-monitor")
    units.programs.nushell.unit
    units.programs.nushell.swizzle.unit
    (home-manager.package "python3")
    units.programs.spotify.unit
    units.programs.starship.unit
    units.programs.sway.unit
    (home-manager.package "termshark")
    units.programs.tmux.unit
    units.programs.viu.unit
    (home-manager.package "viddy")
    units.programs.wezterm.unit
    (home-manager.package "whois")
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
