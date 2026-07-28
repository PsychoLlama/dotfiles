{ lib, pkgs, ... }:

{
  modules.nixos = {
    boot = {
      initrd.supportedFilesystems = [ "zfs" ];
      supportedFilesystems = [ "zfs" ];

      # Don't force-import a pool that wasn't cleanly exported. Reduces the risk
      # of data loss; this is the default from 26.11 onward.
      zfs.forceImportRoot = false;

      # Use stable kernel. This is the default, but I'd rather be explicit.
      # Using non-LTS kernels may corrupt the drive since ZFS has spotty
      # support for newer kernels.
      kernelPackages = pkgs.linuxPackages;

      # ZFS doesn't support freeze/thaw APIs. Hibernation could corrupt files.
      # https://github.com/openzfs/zfs/issues/260
      #
      # `mkBefore` pins it to the front of the command line. The kernel is
      # indifferent to the order, but without an anchor the params reshuffle
      # whenever a module moves in the fixpoint, which rewrites the boot entry.
      kernelParams = lib.mkBefore [
        "nohibernate"
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
