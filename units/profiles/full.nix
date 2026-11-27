{
  units,
  self,
  module,
  ...
}:

let
  inherit (units.platform) home-manager nixos;
in

{
  main = module.component "main" {
    config = [

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
  };

  unit = module.group "unit" {
    config = [
      self.main
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
      (home-manager.lib.package "dive")
      (home-manager.lib.package "dix")
      (home-manager.lib.package "doggo")
      units.editor.unit
      units.editor.profiles.full.unit
      units.programs.fd.unit
      (home-manager.lib.program "fzf")
      units.programs.gh.unit
      units.programs.git.unit
      units.programs.glow.unit
      (home-manager.lib.program "jq")
      (home-manager.lib.package "miniserve")
      (home-manager.lib.package "nix-output-monitor")
      units.programs.nushell.unit
      units.programs.nushell.swizzle.unit
      (home-manager.lib.package "python3")
      units.programs.spotify.unit
      units.programs.starship.unit
      units.programs.sway.unit
      (home-manager.lib.package "termshark")
      units.programs.tmux.unit
      units.programs.viu.unit
      (home-manager.lib.package "viddy")
      units.programs.wezterm.unit
      (home-manager.lib.package "whois")
      units.programs.wireshark.unit
      (home-manager.lib.program "zoxide")
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
  };
}
