{ config, lib, pkgs, ... }:

with lib;

let cfg = config.programs.presets.sshfs;

in {
  options.programs.presets.sshfs.enable = mkEnableOption "Install and configure sshfs";

  # Not enabled by default on macOS (no support for FUSE).
  config.programs.sshfs = mkIf cfg.enable {
    enable = pkgs.stdenv.isLinux;
    package = pkgs.unstable.sshfs;
  };
}
