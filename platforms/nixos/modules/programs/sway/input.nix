{ config, lib, ... }:

let
  inherit (lib) types;
  cfg = config.programs.sway;
  generateSwayInputsConfig =
    inputs:
    let
      inputDefinitions = lib.mapAttrsToList (
        inputName: inputs:
        let
          fieldDefinitions = lib.mapAttrsToList (
            fieldName: field: "${fieldName} ${builtins.toString field}"
          ) inputs;
        in
        ''
          input "${inputName}" {
            ${lib.concatStringsSep "\n  " fieldDefinitions}
          }
        ''
      ) inputs;
    in
    lib.concatStringsSep "\n" inputDefinitions;
in

{
  options.programs.sway.input = lib.mkOption {
    type = types.attrsOf (
      types.attrsOf (
        types.oneOf [
          types.str
          types.int
        ]
      )
    );
    description = "Settings for Sway input devices";
    example = lib.literalExpression ''
      sway.input."AT_Keyboard" = {
        repeat_delay = 250;
      };
    '';
  };

  config.programs.sway.extraConfig = generateSwayInputsConfig cfg.input;
}
