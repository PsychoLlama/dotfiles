{ config, unstable, lib, ... }:

let
  df = config.dotfiles;
  cfg = config.dotfiles.desktop;

in {
  options.dotfiles.desktop = with lib; {
    enable = mkOption {
      type = types.bool;
      description = "Enable a desktop environment";
      default = df.kitchen-sink.enable;
    };

    sway.config = mkOption {
      type = types.path;
      description = "Set the XMonad config file";
      default = ../config/sway.conf;
    };

    rofi.config = mkOption {
      type = types.path;
      description = "Set the Rofi configuration directory";
      default = ../config/rofi;
    };

    waybar = {
      enable = mkOption {
        type = types.bool;
        description = "Enable waybar";
        default = df.kitchen-sink.enable;
      };

      config = mkOption {
        type = types.path;
        description = "Waybar config file";
        default = ../config/waybar/config.json;
      };

      style = mkOption {
        type = types.path;
        description = "Waybar styles";
        default = ../config/waybar/style.css;
      };
    };
  };

  config = with lib;
    mkMerge [
      (mkIf cfg.enable {
        # Enable a minimal desktop environment with Sway/Wayland.
        programs.sway = {
          enable = true;
          extraSessionCommands = ''
            # Remap caps lock to escape.
            export XKB_DEFAULT_OPTIONS=caps:escape
          '';
        };

        environment.systemPackages = with unstable; [
          (callPackage ../pkgs/rofi.nix { configDir = cfg.rofi.config; })
          mako
        ];

        environment.etc."sway/config".source = cfg.sway.config;

        services.greetd = {
          enable = true;
          settings.default_session = {
            user = "greeter";
            command = "${
                lib.makeBinPath [ unstable.greetd.tuigreet ]
              }/tuigreet --asterisks -trc sway";
          };
        };

        # Avoids interleaving with systemd output.
        systemd.services.greetd.serviceConfig.Type = "idle";
        services.greetd.vt = 2;

        # If they're enabling a desktop, these seem like reasonable defaults.
        services.printing.enable = mkDefault true;
        sound.enable = mkDefault true;
        hardware.pulseaudio.enable = mkDefault true;
      })

      (mkIf cfg.waybar.enable {
        environment.etc."sway/config.d/waybar".text = mkIf cfg.waybar.enable
          (let
            init = unstable.writers.writeBash "init-waybar" ''
              ${unstable.waybar}/bin/waybar \
                --config ${cfg.waybar.config} \
                --style ${cfg.waybar.style}
            '';
          in ''
            default_border none

            bar {
              swaybar_command ${init}
              position top
            }
          '');

        environment.systemPackages = [ unstable.waybar ];
      })
    ];
}
