{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.emacs;
  json = pkgs.formats.json { };
  settings-payload = json.generate "${cfg.settings-var}.json" cfg.settings;
in

{
  options.programs.emacs = {
    settings-var = lib.mkOption {
      type = lib.types.str;
      default = "df/config";
      description = ''
        The variable name to which the Emacs settings will be bound.
        This is used to access the settings in Emacs Lisp.
      '';
    };

    settings = lib.mkOption {
      type = json.type;
      default = { };
      description = ''
        Settings exposed to the user configuration.
        Values are bound to the special variable `${cfg.settings-var}`.
      '';
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

    (defcustom df/config (json-read-file "${settings-payload}")
      "Config passed by Nix."
      :type 'sexp
      :group 'dotfiles)

    ${lib.optionalString (cfg.config-file != null) (builtins.readFile cfg.config-file)}
  '';
}
