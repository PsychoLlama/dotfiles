{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.claude-code;
  json = pkgs.formats.json { };

  rootDir = ".claude/dotfiles";
  scriptsDir = "${rootDir}/bin";
in

{
  options.programs.claude-code = {
    scripts = lib.mkOption {
      default = { };
      description = "An executable script installed where Claude can use it. The attribute name becomes the script name.";

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
                description = "Add this script to the `permissions.allow` list.";
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

    servers = lib.mkOption {
      default = { };
      description = "MCP server configurations. The attribute name becomes the server name.";

      type = lib.types.attrsOf (
        lib.types.submodule (
          { name, ... }:
          {
            options = {
              enable = lib.mkOption {
                type = lib.types.bool;
                default = true;
                description = "Whether to enable this MCP server.";
              };

              settings = lib.mkOption {
                type = json.type;
                default = { };
                description = "MCP server configuration (passed through to mcpServers).";
              };

              permissions.allow = lib.mkOption {
                type = lib.types.listOf lib.types.str;
                default = [ ];
                description = ''
                  Tool names exposed by this server to allow.
                  Each entry generates a `mcp__{server}__{tool}` permission.
                '';
              };
            };
          }
        )
      );
    };
  };

  config = lib.mkIf cfg.enable {
    home.file = lib.mapAttrs' (
      name: script: lib.nameValuePair "${scriptsDir}/${name}" { inherit (script) source; }
    ) cfg.scripts;

    programs.claude-code = {
      mcpServers = lib.pipe cfg.servers [
        (lib.filterAttrs (_: server: server.enable))
        (lib.mapAttrs (_: server: server.settings))
      ];

      settings.permissions.allow =
        (lib.pipe cfg.scripts [
          (lib.filterAttrs (_: script: script.allow))
          (lib.mapAttrsToList (_: script: "Bash(${script.path}:*)"))
        ])
        ++ (lib.pipe cfg.servers [
          (lib.filterAttrs (_: server: server.enable))
          (lib.mapAttrsToList (
            name: server: map (tool: "mcp__${name}__${tool}") server.permissions.allow
          ))
          lib.concatLists
        ]);
    };
  };
}
