{ ... }:

{
  imports = [ ./hardware-configuration.nix ];

  boot = {
    # Use the systemd-boot EFI boot loader.
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
  };

  # Network configuration.
  networking = {
    interfaces.wlp0s20f3.useDHCP = true;
    useDHCP = false; # Deprecated option - explicitly opt out.

    wireless = {
      enable = true;
      interfaces = [ "wlp0s20f3" ];
    };
  };

  # TODO: Manage background images more intelligently.
  # services.xserver.displayManager.lightdm.background = /home/overlord/background-image;

  dotfiles.user = {
    account = "overlord";
    fullName = "Jesse Gibson";
  };

  system.stateVersion = "20.09";
}
