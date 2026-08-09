{ config, ... }:

# Bound out here because the module below shadows `config` with its own.
let
  inherit (config.theme) palette;
in

{
  imports = [ ../../system/theme.nix ];

  flake.modules.homeManager.default =
    {
      config,
      lib,
      pkgs,
      ...
    }:

    let
      # Sway's own idle inhibitor is the module's reason to exist, so key off
      # the service rather than assuming a profile wired it up.
      swayidle = config.services.swayidle;

      # Sway ignores every idle inhibitor that isn't attached to the lock surface
      # while a session lock is held (`sway_idle_inhibit_v1_is_active`), so waybar's
      # built-in `idle_inhibitor` silently stops working the moment the screen
      # locks. Toggling the unit is the only lever that survives the lock screen.
      idleLock = pkgs.writeShellApplication {
        name = "waybar-idle-lock";
        runtimeInputs = [
          pkgs.systemd
          pkgs.procps
        ];

        text = ''
          case "''${1-status}" in
            toggle)
              if systemctl --user --quiet is-active swayidle; then
                systemctl --user stop swayidle
              else
                systemctl --user start swayidle
              fi

              # Redraw now rather than waiting out the poll interval.
              pkill -RTMIN+8 waybar || true
              ;;

            status)
              if systemctl --user --quiet is-active swayidle; then
                printf '{"text":"enabled","alt":"enabled","tooltip":"Automatic lock enabled"}\n'
              else
                printf '{"text":"disabled","alt":"disabled","class":"disabled","tooltip":"Automatic lock disabled"}\n'
              fi
              ;;

            *)
              echo "usage: waybar-idle-lock [status|toggle]" >&2
              exit 1
              ;;
          esac
        '';
      };

      # Convert the color palette to a flat list of colors.
      # { bright-red = "<hex>"; normal-red = "<hex>"; ... }
      colors =
        let
          inherit
            (lib.mapAttrs (
              style: colors: lib.concatMapAttrs (id: color: { ${style + "-" + id} = color; }) colors
            ) palette)
            bright
            normal
            ;
        in
        lib.mergeAttrs bright normal;

      # Convert the color palette to GTK CSS color definitions.
      # "@define-color bright-red <hex>;"
      gtk-css-color-defs = lib.concatStringsSep "\n" (
        lib.mapAttrsToList (name: value: "@define-color ${name} ${value};") colors
      );
    in

    {
      programs.waybar = {
        enable = lib.mkDefault true;
        package = lib.mkDefault pkgs.unstable.waybar;

        style = ''
          ${gtk-css-color-defs}

          @import url("${./waybar.css}");
        '';

        settings.main-bar = {
          layer = "top";
          modules-left = [
            "sway/workspaces"
          ];

          modules-center = [
            "sway/window"
          ];

          modules-right = [
            "network"
            "pulseaudio"
            "backlight"
            "battery"
            "clock"
          ]
          ++ lib.optional swayidle.enable "custom/idle-lock";

          "custom/idle-lock" = lib.mkIf swayidle.enable {
            format = "{icon}";
            return-type = "json";
            exec = "${lib.getExe idleLock} status";
            on-click = "${lib.getExe idleLock} toggle";

            # `signal` handles the redraw after a click; the interval only catches
            # the unit being stopped or started from elsewhere.
            signal = 8;
            interval = 30;

            format-icons = {
              enabled = "";
              disabled = "";
            };
          };

          clock = {
            tooltip-format = ''
              <big>{:%Y %B}</big>
              <tt><small>{calendar}</small></tt>'';
            format = "{:%I:%M}";
            format-alt = "{:%Y-%m-%d}";
          };

          backlight = {
            format = "{percent}% {icon}";
            format-icons = [ "" ];
          };

          battery = {
            states = {
              warning = 20;
              critical = 10;
            };

            format = "{capacity}% {icon}";
            format-charging = "{capacity}% 󰂄";
            format-plugged = "{capacity}% ";
            format-alt = "{time} {icon}";
            format-icons = [
              ""
              ""
              ""
              ""
              ""
            ];
          };

          network = {
            format-wifi = "{essid}";
            format-ethernet = "{ipaddr}/{cidr}";
            format-linked = "{ifname} (No IP)";
            format-disconnected = "Offline";
            tooltip-format = "{ifname}: {ipaddr}/{cidr}";
          };

          pulseaudio = {
            format = "{volume}% {icon} {format_source}";
            format-bluetooth = "{volume}% {icon} {format_source}";
            format-bluetooth-muted = " {icon} {format_source}";
            format-muted = " {format_source}";
            format-source = "{volume}% ";
            format-source-muted = "";
            format-icons = {
              headphone = "";
              default = [
                ""
                ""
                ""
              ];
            };
          };
        };
      };
    };
}
