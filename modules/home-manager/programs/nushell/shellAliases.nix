{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.programs.nushell;

  generateAliases = shellAliases:
    (concatStringsSep "\n"
      (mapAttrsToList (alias: action: "alias ${alias} = ${action}")
        shellAliases));

  # Generate aliases to match other shells. HM doesn't provide this
  # automatically.
  nushellAliases =
    pkgs.writeScript "nushell-aliases" (generateAliases cfg.shellAliases);

in {
  options.programs.nushell.shellAliases = mkOption {
    type = types.attrsOf types.str;
    description = "A map of aliases to their definitions";
    default = { };
    example = literalExpression ''
      {
        g = "git";
        v = "vim";
      }
    '';
  };

  config.programs.nushell.initExtra = mkIf (cfg.shellAliases != { }) ''
    source ${nushellAliases}
  '';
}
