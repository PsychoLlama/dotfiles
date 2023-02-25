{ config, lib, pkgs, ... }:

with lib;

let cfg = config.presets.waybar;

in {
  options.presets.waybar.enable = mkEnableOption "Install and configure Waybar";

  config.programs.waybar = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.waybar;
    style = ../../../../config/waybar.css;

    settings.main-bar = {
      layer = "top";
      modules-left = [ "sway/workspaces" ];
      modules-center = [ "sway/window" ];
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
        format-icons = [ "" ];
      };

      battery = {
        states = {
          warning = 20;
          critical = 10;
        };

        format = "{capacity}% {icon}";
        format-charging = "{capacity}% ";
        format-plugged = "{capacity}% ";
        format-alt = "{time} {icon}";
        format-icons = [ "" "" "" "" "" ];
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
        format-bluetooth-muted = " {icon} {format_source}";
        format-muted = " {format_source}";
        format-source = "{volume}% ";
        format-source-muted = "";
        format-icons = {
          headphone = "";
          default = [ "" "" "" ];
        };
      };
    };
  };
}
