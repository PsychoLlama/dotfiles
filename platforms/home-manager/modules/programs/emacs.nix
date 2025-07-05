{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.emacs;
  json = pkgs.formats.json { };
  enabledPluginSet = lib.filterAttrs (name: plugin: plugin.enable) cfg.plugins;
  enabledPluginPackages = lib.mapAttrsToList (_: plugin: plugin.package) enabledPluginSet;
  pluginConfigs = lib.pipe enabledPluginSet [
    (lib.filterAttrs (_: plugin: plugin.config-file != null))
    (lib.mapAttrsToList (name: plugin: ''(load-file "${plugin.config-file}")''))
    (lib.concatStringsSep "\n")
  ];
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

    plugins = lib.mkOption {
      default = { };
      description = ''
        A set of Emacs packages associated with optional config files.
        Packages can be disabled or overridden.
      '';

      type = lib.types.attrsOf (
        lib.types.submodule (
          { name, ... }:
          {
            options.enable = lib.mkEnableOption "Enable the ${name} plugin.";
            options.package = lib.mkPackageOption pkgs.unstable.emacsPackages name { };
            options.config-file = lib.mkOption {
              type = lib.types.nullOr lib.types.path;
              default = null;
              description = ''
                Path to the configuration file for the package.
                This file is loaded by Emacs at startup after `init.el`.
              '';
            };
          }
        )
      );
    };
  };

  config.programs.emacs = {
    extraPackages = (_: enabledPluginPackages);

    extraConfig = ''
      ;; Inject variables from Nix
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

      ;; Main config file (inlined)
      ${lib.optionalString (cfg.config-file != null) (builtins.readFile cfg.config-file)}

      ;; Load plugin configurations
      ${pluginConfigs}
    '';
  };
}
