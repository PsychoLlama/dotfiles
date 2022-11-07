{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.w3m;
  emptyFile = pkgs.writeText "empty-file" "";
  keymapsToFile = keymaps:
    concatStringsSep "\n" (attrValues
      (mapAttrs (binding: action: "keymap ${binding} ${action}") keymaps));

in {
  options.programs.w3m = {
    enable = mkEnableOption "Enable the w3m terminal web browser";
    package = mkPackageOption pkgs "w3m" { };

    extraConfig = mkOption {
      type = types.lines;
      description = "Configuration to include in the w3m config file";
      default = "";
    };

    keybindings = mkOption {
      type = types.attrsOf (types.nullOr types.str);
      description = "Custom keybindings for w3m";
      default = emptyFile;
      example = literalExpression ''
        {
          gg = "BEGIN";
          G = "END";
        }
      '';
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];

    home.file = {
      ".w3m/keymap".text = keymapsToFile cfg.keybindings;
      ".w3m/config".text = cfg.extraConfig;
    };
  };
}
