{ config, lib, ... }:

let
  cfg = config.psychollama.presets.programs.waybar;

  # Convert the color palette to a flat list of colors.
  # { bright-red = "<hex>"; normal-red = "<hex>"; ... }
  colors =
    let
      inherit
        (lib.mapAttrs (
          style: colors: lib.concatMapAttrs (id: color: { ${style + "-" + id} = color; }) colors
        ) config.theme.palette)
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
  config.programs.waybar = lib.mkIf cfg.enable {
    style = ''
      ${gtk-css-color-defs}

      @import url("${./waybar.css}");
    '';

    settings.main-bar = {
      layer = "top";
      modules-left = [
        "sway/workspaces"
        "hyprland/workspaces"
      ];

      modules-center = [
        "sway/window"
        "hyprland/window"
      ];

      modules-right = [
        "network"
        "pulseaudio"
        "backlight"
        "battery"
        "clock"
        "idle_inhibitor"
      ];

      idle_inhibitor = {
        format = "{icon}";
        tooltip-format-activated = "Automatic lock enabled";
        tooltip-format-deactivated = "Automatic lock disabled";
        format-icons = {
          activated = "";
          deactivated = "";
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
}
