{ lib, config, ... }:

let
  inherit (lib) types;
  cfg = config.lsp;
in

{
  options = {
    core.lsp.servers = lib.mkOption {
      type = types.anything;
      readOnly = true;
      internal = true;
      default = lib.mapAttrs (
        _: server: lib.filterAttrs (_: v: v != null) (removeAttrs server [ "enabled" ])
      ) (lib.filterAttrs (_: server: server.enabled) cfg.servers);
      description = "Enabled language server configs passed to vim.lsp.config().";
    };

    lsp = {
      enable = lib.mkEnableOption "Manage LSP clients declaratively";

      servers = lib.mkOption {
        default = { };
        description = ''
          Configuration for neovim LSP clients. Fields mirror `vim.lsp.Config`
          (see `:help vim.lsp.Config`), so presets can set `cmd`,
          `root_markers`, `filetypes`, and `settings` directly.
        '';

        type = types.attrsOf (
          types.submodule {
            options.enabled = lib.mkOption {
              type = types.bool;
              default = true;
              description = ''
                Enable the language server.
              '';
            };

            options.cmd = lib.mkOption {
              type = types.listOf (types.either types.str types.path);
              description = ''
                Command used to launch the language server. First element is
                the executable; remaining elements are passed as arguments.
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

            options.root_markers = lib.mkOption {
              type = types.listOf types.str;
              default = [ ];
              description = ''
                Search upward for files or directories matching these markers.
                If found, the directory becomes the workspace root for the
                language server.
              '';
            };
          }
        );
      };
    };
  };
}
