{ config, pkgs, lib, ... }:

let
  df = config.dotfiles;
  cfg = config.dotfiles.desktop;
  generateSwayInputsConfig = inputs:
    let
      inputDefinitions = lib.mapAttrsToList (inputName: inputs:
        let
          fieldDefinitions = lib.mapAttrsToList
            (fieldName: field: "${fieldName} ${builtins.toString field}")
            inputs;

        in ''
          input "${inputName}" {
            ${lib.concatStringsSep "\n" fieldDefinitions}
          }
        '') inputs;

    in lib.concatStringsSep "\n" inputDefinitions;

in with lib; {
  options.dotfiles.desktop = {
    enable = mkOption {
      type = types.bool;
      description = "Enable a desktop environment";
      default = df.kitchen-sink.enable;
    };

    sway = {
      config = mkOption {
        type = types.path;
        description = "Set the Sway config file";
        default = ../config/sway.conf;
      };

      input = mkOption {
        type =
          types.attrsOf (types.attrsOf (types.oneOf [ types.str types.int ]));
        description = "Settings for Sway input devices";
        example = literalExpression ''
          dotfiles.desktop.sway.input."AT_Keyboard" = {
            repeat_delay = 250;
          };
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    # Enable a minimal desktop environment with Sway/Wayland.
    programs.sway.enable = true;
    dotfiles.user.packages = [ pkgs.unstable.wlsunset ];

    environment.etc."sway/config".source = cfg.sway.config;
    environment.etc."sway/config.d/inputs".text =
      generateSwayInputsConfig cfg.sway.input;
  };
}
