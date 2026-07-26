import ../vim-plugin.nix "nvim-lint" (
  # Linter commands are store paths, so the settings resolve against the
  # package set instead of being a literal.
  { pkgs, ... }:

  {
    extraConfig = ./config.lua;

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
  }
)
