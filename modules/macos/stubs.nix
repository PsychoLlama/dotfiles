{ lib, ... }:

# HACK: Some of my toolkits assume a NixOS environment. This stubs it out for
# Mac. At some point, these code paths should split and attempt to share less.

with lib; {
  options = {
    console.font = mkOption { type = types.str; };
    hardware.pulseaudio.enable = mkOption { type = types.bool; };
    networking.networkmanager.enable = mkOption { type = types.bool; };
    programs.wireshark = { enable = mkEnableOption "Sway"; };
    security.sudo.wheelNeedsPassword = mkEnableOption "wheel pass";
    programs.zsh.histSize = mkOption { type = types.int; };

    services.printing.enable = mkEnableOption "printing";
    sound.enable = mkEnableOption "sound";
    systemd.services = mkOption { type = types.submodule { }; };

    programs.sway = {
      enable = mkEnableOption "Sway";
      extraSessionCommands = mkOption { type = types.str; };
    };

    services.greetd = {
      enable = mkEnableOption "greetd";
      vt = mkOption { type = types.str; };
      settings = mkOption { type = types.anything; };
    };

    virtualisation.docker = {
      enable = mkEnableOption "docker";
      autoPrune = mkOption { type = types.bool; };
      package = mkOption { type = types.derivation; };
    };

    programs.tmux = {
      historyLimit = mkOption { type = types.int; };
      keyMode = mkOption { type = types.str; };
      escapeTime = mkOption { type = types.int; };
      customPaneNavigationAndResize = mkOption { type = types.str; };
    };
  };
}
