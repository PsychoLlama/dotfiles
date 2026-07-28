{ cfg, lib, ... }:

# Spelled out rather than calling `vim-plugin.nix`: the preview server needs
# `withNodeJs`, which sits outside the plugin's own manifest entry.

{
  options.package = lib.mkOption {
    type = lib.types.nullOr lib.types.package;
    default = null;
    defaultText = lib.literalExpression "plugin.pkgs.markdown-preview-nvim";
    description = ''
      Plugin package to install. Null resolves `markdown-preview-nvim` by name
      against the editor's `plugin.sources`.
    '';
  };

  modules.editor =
    { config, ... }:

    {
      # markdown-preview-nvim runs a Node.js server to render the preview.
      withNodeJs = true;

      plugins.markdown-preview-nvim = {
        enable = lib.mkDefault true;
        package = lib.mkDefault (
          if cfg.package == null then config.plugin.pkgs.markdown-preview-nvim else cfg.package
        );

        extraConfig = ./config.lua;

        # Defer the markdown preview until markdown files are opened
        defer.ft = "markdown";
      };
    };
}
