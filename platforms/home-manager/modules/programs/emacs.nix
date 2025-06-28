{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.emacs;
  json = pkgs.formats.json { };
in

{
  options.programs.emacs = {
    variables = lib.mkOption {
      type = lib.types.attrsOf (
        lib.types.submodule {
          options.value = lib.mkOption {
            type = json.type;
            default = null;
            description = "Default value for the variable.";
          };

          options.description = lib.mkOption {
            type = lib.types.str;
            default = "Config variable set by Nix.";
            description = "Doc string for the variable.";
          };
        }
      );

      default = { };
      description = ''
        A set of variables defined in Emacs.
        All variables belong to the `dotfiles` group.
      '';

      example = {
        "df/manage-clipboard" = {
          value = true;
          description = "Manage the system clipboard.";
        };
      };
    };

    config-file = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = null;
      description = ''
        Path to the Emacs configuration file.
        This file is loaded by Emacs at startup.
      '';
    };
  };

  config.programs.emacs.extraConfig = ''
    (defgroup dotfiles nil
      "Settings for the dotfiles framework.")

    ${lib.pipe cfg.variables [
      (lib.mapAttrsToList (
        name: var: ''
          (defcustom ${name} (json-read-file "${json.generate "${name}.json" var.value}")
            ${lib.strings.toJSON var.description}
            :type 'sexp
            :group 'dotfiles)
        ''
      ))

      (lib.concatStringsSep "\n")
    ]}

    ${lib.optionalString (cfg.config-file != null) (builtins.readFile cfg.config-file)}
  '';
}
