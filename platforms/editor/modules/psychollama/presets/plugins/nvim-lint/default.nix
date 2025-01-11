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
      };

      linters_by_ft = rec {
        sh = bash;
        bash = [ "shellcheck" ];
        vim = [ "vint" ];

        javascript = typescript;
        javascriptreact = typescript;
        typescriptreact = typescript;
        typescript = [ "eslint_d" ];
      };
    };
  };
}
