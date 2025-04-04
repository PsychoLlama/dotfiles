{
  lib,
  config,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.plugins.nvim-lint;
in

{
  config.plugins.nvim-lint = lib.mkIf cfg.enable {
    opts = {
      linters = {
        eslint_d.cmd = "${pkgs.unstable.eslint_d}/bin/eslint_d";
        shellcheck.cmd = "${pkgs.unstable.shellcheck}/bin/shellcheck";

        # Built-in vint config does not support stdin.
        vint = {
          cmd = "${pkgs.unstable.vim-vint}/bin/vint";
          stdin = true;
          args = [
            "--enable-neovim"
            "--style-problem"
            "--json"
            "-"
          ];
        };

        # Assumes this is installed in the local dev shell.
        luacheck.cmd = "luacheck";
      };

      linters_by_ft = rec {
        sh = bash;
        bash = [ "shellcheck" ];
        vim = [ "vint" ];
        lua = [ "luacheck" ];
        rust = [ "clippy" ];

        javascript = typescript;
        javascriptreact = typescript;
        typescriptreact = typescript;
        typescript = [ "eslint_d" ];
      };
    };
  };
}
