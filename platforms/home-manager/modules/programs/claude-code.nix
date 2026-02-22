{
  config,
  lib,
  ...
}:

let
  cfg = config.programs.claude-code;

  rootDir = ".claude/dotfiles";
  scriptsDir = "${rootDir}/bin";
in

{
  options.programs.claude-code.scripts = lib.mkOption {
    default = { };
    description = ''
      An executable script installed where Claude can use it.
      The attribute name becomes the script name.
    '';

    type = lib.types.attrsOf (
      lib.types.submodule (
        { name, ... }:
        {
          options = {
            source = lib.mkOption {
              type = lib.types.path;
              description = "Executable script";
            };

            allow = lib.mkOption {
              type = lib.types.bool;
              default = false;
              description = ''
                Add this script to the `permissions.allow` list.
              '';
            };

            path = lib.mkOption {
              type = lib.types.str;
              readOnly = true;
              description = "Full path to the installed script.";
              default = "~/${scriptsDir}/${name}";
            };
          };
        }
      )
    );
  };

  config = lib.mkIf cfg.enable {
    home.file = lib.mapAttrs' (
      name: script: lib.nameValuePair "${scriptsDir}/${name}" { inherit (script) source; }
    ) cfg.scripts;

    programs.claude-code.settings.permissions.allow = lib.pipe cfg.scripts [
      (lib.filterAttrs (_: script: script.allow))
      (lib.mapAttrsToList (_: script: "Bash(${script.path}:*)"))
    ];
  };
}
