{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.services.zfs;
in

{
  options.psychollama.presets.services.zfs = {
    enable = lib.mkEnableOption "Enable ZFS and perform automatic maintenance";
  };

  config = lib.mkIf cfg.enable {
    boot = {
      initrd.supportedFilesystems = [ "zfs" ];
      supportedFilesystems = [ "zfs" ];
      kernelPackages = pkgs.linuxPackages_6_6; # Latest compatible with ZFS.

      # ZFS doesn't support freeze/thaw APIs. Hibernation could corrupt files.
      # https://github.com/openzfs/zfs/issues/260
      #
      # Also, set a maximum size on the ZFS Adaptive Replacement Cache (1GB).
      kernelParams = [
        "nohibernate"
        "zfs.zfs_arc_max=1073741824"
      ];

      loader = {
        # Fixes a potential issue where too many hardlinks in the nix store can
        # brick the boot process. Recommended for ZFS.
        grub.copyKernels = true;
        efi.canTouchEfiVariables = true;
      };
    };

    services.zfs = {
      trim.enable = true;
      autoScrub.enable = true;
      autoSnapshot = {
        enable = true;
        flags = "-k -p --utc";
      };
    };
  };
}
