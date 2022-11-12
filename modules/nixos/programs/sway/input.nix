{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.programs.sway;
  generateSwayInputsConfig = inputs:
    let
      inputDefinitions = mapAttrsToList (inputName: inputs:
        let
          fieldDefinitions = mapAttrsToList
            (fieldName: field: "${fieldName} ${builtins.toString field}")
            inputs;

        in ''
          input "${inputName}" {
            ${concatStringsSep "\n" fieldDefinitions}
          }
        '') inputs;

    in concatStringsSep "\n" inputDefinitions;

in {
  options.programs.sway.input = mkOption {
    type = types.attrsOf (types.attrsOf (types.oneOf [ types.str types.int ]));
    description = "Settings for Sway input devices";
    example = literalExpression ''
      sway.input."AT_Keyboard" = {
        repeat_delay = 250;
      };
    '';
  };

  config.environment.etc."sway/config.d/input".text =
    mkIf cfg.enable (generateSwayInputsConfig cfg.input);
}
