{
  pkgs,
  lib,
  config,
  ...
}:

let
  inherit (lib) types;
  cfg = config.lsp;
  lua = lib.generators.toLua { };
  servers = lib.filterAttrs (_: server: server.enabled) cfg.servers;
in

{
  options.lsp = {
    enable = lib.mkEnableOption "Manage LSP clients declaratively";

    servers = lib.mkOption {
      default = { };
      description = ''
        Configuration for neovim LSP clients.
      '';

      type = types.attrsOf (
        types.submodule (
          { name, ... }:
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

            options.command = lib.mkOption {
              type = types.listOf (types.either types.str types.path);
              default = [ ];
              description = ''
                Command to start the language server.
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
              type = types.attrsOf types.anything;
              default = { };
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
    source ${pkgs.writeText "lsp.lua" ''
      require('editor.lsp').setup(${lua servers})
    ''}
  '';
}
