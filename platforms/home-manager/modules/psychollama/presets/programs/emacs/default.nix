{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.programs.emacs;
in

{
  options.psychollama.presets.programs.emacs = {
    enable = lib.mkEnableOption "Use an opinionated Emacs config";
  };

  config.programs.emacs = lib.mkIf cfg.enable {
    enable = true;
    package = lib.mkDefault pkgs.unstable.emacs;
    config-file = ./emacs.el;
    plugins = {
      # Major modes
      just-ts-mode.enable = lib.mkDefault true;
      lua-mode.enable = lib.mkDefault true;
      markdown-mode.enable = lib.mkDefault true;
      nix-ts-mode.enable = lib.mkDefault true;
      nushell-ts-mode.enable = lib.mkDefault true;
      rust-mode.enable = lib.mkDefault true;

      # Core features
      direnv.enable = lib.mkDefault true;
      aidermacs.enable = lib.mkDefault true;
      apheleia.enable = lib.mkDefault true;
      company.enable = lib.mkDefault true;
      copilot = {
        enable = lib.mkDefault true;
        package = pkgs.unstable.emacsPackages.copilot.override {
          copilot-language-server-fhs =
            if pkgs.stdenv.isDarwin then
              # FHS variant is not supported (or necessary) on macOS.
              pkgs.unstable.copilot-language-server
            else
              pkgs.unstable.copilot-language-server-fhs;
        };
      };
      counsel.enable = lib.mkDefault true;
      counsel-fd.enable = lib.mkDefault true;
      counsel-projectile.enable = lib.mkDefault true;
      diredfl.enable = lib.mkDefault true;
      doom-themes.enable = lib.mkDefault true;
      eglot.enable = lib.mkDefault true;
      evil.enable = lib.mkDefault true;
      evil-collection.enable = lib.mkDefault true;
      evil-commentary.enable = lib.mkDefault true;
      evil-surround.enable = lib.mkDefault true;
      evil-terminal-cursor-changer.enable = lib.mkDefault true;
      flycheck.enable = lib.mkDefault true;
      flycheck-eglot.enable = lib.mkDefault true;
      gptel.enable = lib.mkDefault true;
      magit.enable = lib.mkDefault true;
      paredit.enable = lib.mkDefault true;
      projectile.enable = lib.mkDefault true;
      rainbow-delimiters.enable = lib.mkDefault true;
      treesit-grammars = {
        enable = lib.mkDefault true;
        package = pkgs.unstable.emacsPackages.treesit-grammars.with-all-grammars;
      };
      undo-tree.enable = lib.mkDefault true;
      xclip.enable = lib.mkDefault true;
    };

    # TODO: Make these packages configurable.
    variables = {
      # Formatters
      "my/formatter-prettier" = {
        description = "Executable for `prettierd`.";
        value = "${pkgs.unstable.prettierd}/bin/prettierd";
      };

      "my/formatter-eslint" = {
        description = "Executable for `eslint_d`.";
        value = "${pkgs.unstable.eslint_d}/bin/eslint_d";
      };

      "my/formatter-nixfmt" = {
        description = "Executable for `nixfmt`.";
        value = "${pkgs.unstable.nixfmt-rfc-style}/bin/nixfmt";
      };

      "my/formatter-stylua" = {
        description = "Executable for `stylua`.";
        value = "${pkgs.unstable.stylua}/bin/stylua";
      };

      # Language servers
      "my/lsp-nil" = {
        description = "Executable for the Nil (nix) language server.";
        value = "${pkgs.unstable.nil}/bin/nil";
      };

      "my/lsp-tsserver" = {
        description = "Executable for the TypeScript language server.";
        value = "${pkgs.unstable.nodePackages.typescript-language-server}/bin/typescript-language-server";
      };

      "my/lsp-clangd" = {
        description = "Executable for the Clangd language server.";
        value = if pkgs.stdenv.isDarwin then "clangd" else "${pkgs.unstable.clang-tools}/bin/clangd";
      };

      "my/lsp-gopls" = {
        description = "Executable for the Go language server.";
        value = "${pkgs.unstable.gopls}/bin/gopls";
      };

      "my/lsp-rust-analyzer" = {
        description = "Executable for the Rust Analyzer language server.";
        value = "${pkgs.unstable.rust-analyzer}/bin/rust-analyzer";
      };

      "my/lsp-luals" = {
        description = "Executable for the Lua language server.";
        value = "${pkgs.unstable.lua-language-server}/bin/lua-language-server";
      };

      "my/lsp-jsonls" = {
        description = "Executable for the JSON language server.";
        value = "${pkgs.unstable.vscode-langservers-extracted}/bin/vscode-json-language-server";
      };

      # Linters
      "my/linter-shellcheck" = {
        description = "Executable for `shellcheck`.";
        value = "${pkgs.unstable.shellcheck}/bin/shellcheck";
      };

      "my/linter-eslint" = {
        description = "Executable for `eslint_d`.";
        value = "${pkgs.unstable.eslint_d}/bin/eslint_d";
      };

      # Assumes this is installed in the local dev shell.
      "my/linter-luacheck" = {
        description = "Executable for `luacheck`.";
        value = "luacheck";
      };
    };
  };
}
