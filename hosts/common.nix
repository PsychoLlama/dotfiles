{ pkgs, unstable, inputs, ... }:

{
  # This can be removed once nix flakes ship standard.
  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  # Internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = "America/Los_Angeles";

  # Set system fonts.
  fonts.enableDefaultFonts = true;
  console.keyMap = "us";

  hardware.bluetooth.enable = true;
}
