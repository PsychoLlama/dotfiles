{ pkgs, unstable, inputs, ... }:

{
  # This can be removed once nix flakes ship standard.
  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';

    # Add a pinned `unstable` flake to the registry. Prevents Nix from
    # downloading the nixpkgs repo with every new commit.
    registry.unstable.flake = inputs.nixpkgs-unstable;
  };

  # Install docker and run it automatically as a daemon.
  virtualisation.docker = {
    enable = true;
    package = unstable.docker;
    autoPrune.enable = true;
  };

  # Internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = "America/Los_Angeles";

  # Set system fonts.
  fonts = {
    enableDefaultFonts = true;
    fonts = [unstable.fira-code];
  };

  console = {
    font = "Fira Code";
    keyMap = "us";
  };

  # Misc hardware and drivers.
  services.printing.enable = true;
  sound.enable = true;
  hardware = {
    pulseaudio.enable = true;
    bluetooth.enable = true;
  };

  # Remap caps to escape. Honestly, who uses that key?
  services.xserver.xkbOptions = "caps:escape";
}
