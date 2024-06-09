{ lib, config, ... }:

let
  inherit (lib) types;
  cfg = config.lsp;
  lua = lib.generators.toLua { };
  servers = lib.filterAttrs (_: server: server.enabled) cfg.servers;
in

{
  imports = [ ./efm.nix ];

  options.lsp = {
    enable = lib.mkEnableOption "Manage LSP clients declaratively";

    servers = lib.mkOption {
      default = { };
      description = ''
        Configuration for neovim LSP clients.
      '';

      type = types.attrsOf (
        types.submodule (
          { name, config, ... }:
          {
            options.enabled = lib.mkOption {
              type = types.bool;
              default = true;
              description = ''
                Enable the language server.
              '';
            };

            options.name = lib.mkOption {
              type = types.str;
              default = name;
              description = ''
                Unique name for the client. Clients with the same name and
                root directory are shared across buffers.
              '';
            };

            options.server = lib.mkOption {
              type = types.str;
              description = ''
                Binary that starts the language server.
              '';
            };

            options.args = lib.mkOption {
              type = types.listOf (types.either types.str types.path);
              default = [ ];
              description = ''
                Arguments to pass to the language server. It must listen on
                stdin.
              '';
            };

            options.command = lib.mkOption {
              type = types.listOf (types.either types.str types.path);
              default = [ config.server ] ++ config.args;
              readOnly = true;
              description = ''
                Generated command that executes the language server.
              '';
            };

            options.filetypes = lib.mkOption {
              type = types.listOf types.str;
              default = [ ];
              description = ''
                Automatically attach the language server for these filetypes.
              '';
            };

            options.settings = lib.mkOption {
              type = types.nullOr (types.attrsOf types.anything);
              default = null;
              description = ''
                Settings to pass to the language server.
              '';
            };

            options.root.patterns = lib.mkOption {
              type = types.listOf types.str;
              default = [ ];
              description = ''
                Search upward for files or directories matching these
                patterns. If found, it becomes the workspace for the language
                server.
              '';
            };
          }
        )
      );
    };
  };

  # TODO: Integrate `editor.lsp` into Nix+Neovim framework.
  config.extraConfig = lib.mkIf cfg.enable ''
    require('editor.lsp').setup(${lua servers})
  '';
}
