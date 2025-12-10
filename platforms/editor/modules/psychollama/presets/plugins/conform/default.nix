{
  lib,
  config,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.plugins.conform-nvim;
in

{
  config.plugins.conform-nvim = lib.mkIf cfg.enable {
    opts = {
      format_on_save = true;
      default_format_opts.undojoin = true;

      formatters = {
        nixfmt = {
          command = "${pkgs.unstable.nixfmt-rfc-style}/bin/nixfmt";
          args = [ "--quiet" ];
        };

        prettier = {
          command = "${pkgs.unstable.prettierd}/bin/prettierd";
          args = [ "$FILENAME" ];
        };

        eslint = {
          command = "${pkgs.unstable.eslint_d}/bin/eslint_d";
          args = [
            "--stdin-filename"
            "$FILENAME"
            "--stdin"
            "--fix-to-stdout"
          ];
        };

        stylua = {
          command = "${pkgs.unstable.stylua}/bin/stylua";
          args = [
            "--search-parent-directories"
            "--allow-hidden"
            "--stdin-filepath"
            "$FILENAME"
            "-"
          ];
        };

        # Dynamic dependency. Assumes `rustfmt` is provided by direnv and
        # so will only work if neovim was spawned from the project.
        rustfmt = {
          command = "rustfmt";
          args = [ "--emit=stdout" ];
        };

        # Dynamic dependency. Assumes `gofmt` is provided by direnv.
        gofmt = {
          command = "gofmt";
        };

      };

      formatters_by_ft = rec {
        javascript = typescript;
        javascriptreact = typescript;
        typescriptreact = typescript;
        typescript = [
          "eslint"
          "prettier"
        ];

        css = [ "prettier" ];
        graphql = [ "prettier" ];
        html = [ "prettier" ];
        json = [ "prettier" ];
        json5 = [ "prettier" ];
        jsonc = [ "prettier" ];
        less = [ "prettier" ];
        lua = [ "stylua" ];
        markdown = [ "prettier" ];
        nix = [ "nixfmt" ];
        rust = [ "rustfmt" ];
        vue = [ "prettier" ];
        yaml = [ "prettier" ];
        go = [ "gofmt" ];
      };
    };
  };
}
