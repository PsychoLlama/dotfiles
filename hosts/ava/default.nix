{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
  ];

  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  boot = {
    # Use the systemd-boot EFI boot loader.
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    kernelPackages = pkgs.linuxPackages_latest;
  };

  # Network configuration.
  networking = {
    wireless.enable = true;
    hostName = "ava";

    wireless.interfaces = ["wlp0s20f3"];

    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    useDHCP = false;
    interfaces.wlp0s20f3.useDHCP = true;
  };

  # TODO: Manage background images more intelligently.
  # services.xserver.displayManager.lightdm.background = /home/overlord/background-image;

  dotfiles.user = {
    account = "overlord";
    fullName = "Jesse Gibson";
  };

  system.stateVersion = "20.09";
}
