{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.programs.emacs;

  /**
    Similar to `lib.mkPackageOption` but supports pulling executables
    from the path instead of nixpkgs.
  */
  mkExeOption =
    package: executable:
    lib.mkOption {
      type = lib.types.str;
      default = if package == null then executable else "${package}/bin/${executable}";
      description = "Executable path for `${executable}`.";
    };
in

{
  options.psychollama.presets.programs.emacs = {
    enable = lib.mkEnableOption "Use an opinionated Emacs config";

    linters = {
      eslint = mkExeOption pkgs.unstable.eslint_d "eslint_d";
      ruff = mkExeOption pkgs.unstable.ruff "ruff";
      shellcheck = mkExeOption pkgs.unstable.shellcheck "shellcheck";

      # Assumes this is installed in the local dev shell.
      luacheck = mkExeOption null "luacheck";
    };

    formatters = {
      black = mkExeOption pkgs.unstable.black "black";
      eslint = mkExeOption pkgs.unstable.eslint_d "eslint_d";
      nixfmt = mkExeOption pkgs.unstable.nixfmt-rfc-style "nixfmt";
      prettier = mkExeOption pkgs.unstable.prettierd "prettierd";
      stylua = mkExeOption pkgs.unstable.stylua "stylua";
    };

    languageServers = {
      gopls = mkExeOption pkgs.unstable.gopls "gopls";
      jsonls = mkExeOption pkgs.unstable.vscode-langservers-extracted "vscode-json-language-server";
      luals = mkExeOption pkgs.unstable.lua-language-server "lua-language-server";
      nil = mkExeOption pkgs.unstable.nil "nil";
      pyright = mkExeOption pkgs.unstable.pyright "pyright-langserver";
      rust-analyzer = mkExeOption pkgs.unstable.rust-analyzer "rust-analyzer";
      tsserver = mkExeOption pkgs.unstable.nodePackages.typescript-language-server "typescript-language-server";
      clangd =
        # Use clangd from XCode on macOS.
        if pkgs.stdenv.isDarwin then
          mkExeOption null "clangd"
        else
          mkExeOption pkgs.unstable.clang-tools "clangd";
    };
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
      apheleia.enable = lib.mkDefault true;
      company.enable = lib.mkDefault true;
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

    variables = {
      # Formatters
      "my/formatter-prettier" = {
        description = "Executable for `prettierd`.";
        value = cfg.formatters.prettier;
      };

      "my/formatter-eslint" = {
        description = "Executable for `eslint_d`.";
        value = cfg.formatters.eslint;
      };

      "my/formatter-nixfmt" = {
        description = "Executable for `nixfmt`.";
        value = cfg.formatters.nixfmt;
      };

      "my/formatter-stylua" = {
        description = "Executable for `stylua`.";
        value = cfg.formatters.stylua;
      };

      "my/formatter-black" = {
        description = "Executable for `black`.";
        value = cfg.formatters.black;
      };

      # Language servers
      "my/lsp-nil" = {
        description = "Executable for the Nil (nix) language server.";
        value = cfg.languageServers.nil;
      };

      "my/lsp-tsserver" = {
        description = "Executable for the TypeScript language server.";
        value = cfg.languageServers.tsserver;
      };

      "my/lsp-gopls" = {
        description = "Executable for the Go language server.";
        value = cfg.languageServers.gopls;
      };

      "my/lsp-rust-analyzer" = {
        description = "Executable for the Rust Analyzer language server.";
        value = cfg.languageServers.rust-analyzer;
      };

      "my/lsp-luals" = {
        description = "Executable for the Lua language server.";
        value = cfg.languageServers.luals;
      };

      "my/lsp-jsonls" = {
        description = "Executable for the JSON language server.";
        value = cfg.languageServers.jsonls;
      };

      "my/lsp-clangd" = {
        description = "Executable for the Clangd language server.";
        value = cfg.languageServers.clangd;
      };

      "my/lsp-pyright" = {
        description = "Executable for the Pyright language server.";
        value = cfg.languageServers.pyright;
      };

      # Linters
      "my/linter-shellcheck" = {
        description = "Executable for `shellcheck`.";
        value = cfg.linters.shellcheck;
      };

      "my/linter-eslint" = {
        description = "Executable for `eslint_d`.";
        value = cfg.linters.eslint;
      };

      "my/linter-luacheck" = {
        description = "Executable for `luacheck`.";
        value = cfg.linters.luacheck;
      };

      "my/linter-ruff" = {
        description = "Executable for `ruff`.";
        value = cfg.linters.ruff;
      };
    };
  };

  # Set emacs to be the default editor. The HM module only does this if you
  # enable the launchd service on macOS which is a huge pain to manage.
  #
  # Source:
  # https://github.com/nix-community/home-manager/blob/080e8b48b0318b38143d5865de9334f46d51fce3/modules/services/emacs.nix#L124-L128
  config.home.sessionVariables = lib.mkIf cfg.enable {
    EDITOR = lib.mkDefault (
      lib.getBin (
        pkgs.writeShellScript "editor" ''exec ${lib.getBin config.programs.emacs.package}/bin/emacsclient "''${@:---create-frame}"''
      )
    );
  };
}
