{ config, lib, pkgs, inputs, ... }:

{
  imports = [
    inputs.nixpkgs.nixosModules.notDetected
    inputs.hardware.nixosModules.lenovo-thinkpad-p1-gen3
  ];

  boot.initrd.availableKernelModules =
    [ "xhci_pci" "nvme" "usb_storage" "sd_mod" "sdhci_pci" ];

  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems = let
    zfs = path: {
      device = "locker/${path}";
      fsType = "zfs";
      options = [ "zfsutil" ];
    };

  in {
    # Only the ZFS pool and boot partition are persisted. Everything else is
    # volatile.
    "/" = {
      device = "none";
      fsType = "tmpfs";
    };

    "/boot" = {
      device = "/dev/disk/by-label/boot";
      fsType = "vfat";
    };

    "/nix" = zfs "nixos/nix";
    "/etc" = zfs "nixos/etc";
    "/var" = zfs "nixos/var";
    "/var/lib" = zfs "nixos/var/lib";
    "/var/log" = zfs "nixos/var/log";
    "/var/spool" = zfs "nixos/var/spool";
    "/home" = zfs "data/home";
    "/root" = zfs "data/home/root";
    "/home/overlord" = zfs "data/home/overlord";
  };

  swapDevices = [{
    # This is a stable identifier for /dev/disk/by-label/swap.
    device = "/dev/disk/by-partuuid/dc08f0f5-451d-483d-af2a-efca672f4897";

    # Note: Encryption can potentially corrupt hibernation state. This isn't
    # a problem as hibernation is currently disabled for ZFS support.
    randomEncryption.enable = true;
  }];
}
