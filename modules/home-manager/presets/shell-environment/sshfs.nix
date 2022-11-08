{ config, lib, pkgs, ... }:

let cfg = config.presets.sshfs;

in with lib; {
  options.presets.sshfs.enable = mkEnableOption "Install and configure sshfs";

  # Not enabled by default on macOS (no support for FUSE).
  config.programs.sshfs = mkIf cfg.enable {
    enable = pkgs.stdenv.isLinux;
    package = pkgs.unstable.sshfs;
  };
}
