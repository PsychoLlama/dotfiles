{ config, unstable, lib, ... }:

let cfg = config.dotfiles.desktop;

in {
  options.dotfiles.desktop = with lib; {
    enable = mkOption {
      type = types.bool;
      description = "Enable a desktop environment";
      default = true;
    };

    xmonad.config = mkOption {
      type = types.path;
      description = "Set the XMonad config file";
      default = ../config/xmonad.hs;
    };

    rofi.config = mkOption {
      type = types.path;
      description = "Set the Rofi configuration directory";
      default = ../config/rofi;
    };
  };

  config = with lib; {
    # Enable the X11 windowing system.
    services.xserver = mkIf cfg.enable {
      enable = true;

      # Seems a more reasonable default.
      autoRepeatDelay = mkDefault 250;

      # Honestly, who uses caps lock?
      xkbOptions = mkDefault "caps:escape";

      libinput = {
        enable = mkDefault true;

        # Configure the touchpad.
        touchpad = {
          naturalScrolling = mkDefault true;
          tapping = mkDefault false; # Disable soft tap to click.
        };
      };

      # Swap out the login screen program.
      displayManager.lightdm.greeters.enso.enable = mkDefault true;

      # Use XMonad to manage the graphical environment.
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = mkDefault true;
        config = cfg.xmonad.config;
      };
    };

    # Screen locking utility.
    programs.slock.enable = mkIf cfg.enable true;

    # App launcher.
    environment.systemPackages = mkIf cfg.enable [
      (unstable.callPackage ../pkgs/rofi.nix { configDir = cfg.rofi.config; })
    ];

    # If they're enabling a desktop, these seem like reasonable defaults.
    services.printing.enable = mkIf cfg.enable (mkDefault true);
    sound.enable = mkIf cfg.enable (mkDefault true);
    hardware.pulseaudio.enable = mkIf cfg.enable (mkDefault true);
  };
}
