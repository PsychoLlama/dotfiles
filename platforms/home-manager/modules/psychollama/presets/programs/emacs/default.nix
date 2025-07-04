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
    extraPackages =
      let
        plugins = pkgs.unstable.emacsPackages;
        copilot = plugins.copilot.override {
          copilot-language-server-fhs =
            if pkgs.stdenv.isDarwin then
              # FHS variant is not supported (or necessary) on macOS.
              pkgs.unstable.copilot-language-server
            else
              pkgs.unstable.copilot-language-server-fhs;
        };
      in
      _: [
        # Major modes
        plugins.just-ts-mode
        plugins.lua-mode
        plugins.markdown-mode
        plugins.nix-ts-mode
        plugins.nushell-ts-mode
        plugins.rust-mode

        # Core features
        copilot
        plugins.aidermacs
        plugins.apheleia
        plugins.company
        plugins.counsel
        plugins.counsel-fd
        plugins.counsel-projectile
        plugins.diredfl
        plugins.doom-themes
        plugins.eglot
        plugins.evil
        plugins.evil-collection
        plugins.evil-commentary
        plugins.evil-surround
        plugins.evil-terminal-cursor-changer
        plugins.flycheck
        plugins.flycheck-eglot
        plugins.magit
        plugins.paredit
        plugins.projectile
        plugins.rainbow-delimiters
        plugins.treesit-grammars.with-all-grammars
        plugins.undo-tree
        plugins.xclip
      ];

    # TODO: Make these packages configurable.
    variables = {
      # Formatters
      "df/formatter-prettier" = {
        description = "Executable for `prettierd`.";
        value = "${pkgs.unstable.prettierd}/bin/prettierd";
      };

      "df/formatter-eslint" = {
        description = "Executable for `eslint_d`.";
        value = "${pkgs.unstable.eslint_d}/bin/eslint_d";
      };

      "df/formatter-nixfmt" = {
        description = "Executable for `nixfmt`.";
        value = "${pkgs.unstable.nixfmt-rfc-style}/bin/nixfmt";
      };

      "df/formatter-stylua" = {
        description = "Executable for `stylua`.";
        value = "${pkgs.unstable.stylua}/bin/stylua";
      };

      # Language servers
      "df/lsp-nil" = {
        description = "Executable for the Nil (nix) language server.";
        value = "${pkgs.unstable.nil}/bin/nil";
      };

      "df/lsp-tsserver" = {
        description = "Executable for the TypeScript language server.";
        value = "${pkgs.unstable.nodePackages.typescript-language-server}/bin/typescript-language-server";
      };

      "df/lsp-clangd" = {
        description = "Executable for the Clangd language server.";
        value = if pkgs.stdenv.isDarwin then "clangd" else "${pkgs.unstable.clang-tools}/bin/clangd";
      };

      "df/lsp-gopls" = {
        description = "Executable for the Go language server.";
        value = "${pkgs.unstable.gopls}/bin/gopls";
      };

      "df/lsp-rust-analyzer" = {
        description = "Executable for the Rust Analyzer language server.";
        value = "${pkgs.unstable.rust-analyzer}/bin/rust-analyzer";
      };

      "df/lsp-luals" = {
        description = "Executable for the Lua language server.";
        value = "${pkgs.unstable.lua-language-server}/bin/lua-language-server";
      };

      "df/lsp-jsonls" = {
        description = "Executable for the JSON language server.";
        value = "${pkgs.unstable.vscode-langservers-extracted}/bin/vscode-json-language-server";
      };

      # Linters
      "df/linter-shellcheck" = {
        description = "Executable for `shellcheck`.";
        value = "${pkgs.unstable.shellcheck}/bin/shellcheck";
      };

      "df/linter-eslint" = {
        description = "Executable for `eslint_d`.";
        value = "${pkgs.unstable.eslint_d}/bin/eslint_d";
      };

      # Assumes this is installed in the local dev shell.
      "df/linter-luacheck" = {
        description = "Executable for `luacheck`.";
        value = "luacheck";
      };
    };
  };
}
