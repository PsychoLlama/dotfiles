# Every overlay the package set is built with.
inputs:

let
  overlays = {
    # Upgrade a number of packages to their bleeding edge versions.
    unstable-packages = final: prev: {
      # Provides `pkgs.unstable`.
      unstable = import inputs.nixpkgs-unstable {
        inherit (prev.stdenv.hostPlatform) system;
        inherit (prev) config;

        # Every overlay but this one, which would recurse forever.
        overlays = [
          overlays.custom-packages
          overlays.custom-patches
          overlays.vim-plugins
        ];
      };
    };

    # Third-party packages built from source.
    custom-packages = final: prev: {
      # Custom packages live under their own namespace so they never shadow (or get
      # shadowed by) upstream attributes on `pkgs.*`/`pkgs.unstable.*`.
      custom = (prev.custom or { }) // {
        chrome-devtools-mcp = final.callPackage ./chrome-devtools-mcp { };
        claude-code-bin = final.callPackage ./claude-code { };
        codex-bin = final.callPackage ./codex { };
        nvim-rs = final.wrapNeovim (final.callPackage ./nvim-rs { }) { };
      };
    };

    # Patches and overrides for broken upstream packages.
    custom-patches = final: prev: {
      # The 4.10.0 server bundles open with a CommonJS `require("core-js/...")`
      # prelude but also contain an `import.meta` shim further down. Node's module
      # auto-detection sees `import.meta`, loads each *ServerMain.js as ESM, and the
      # bare top-level `require` is then undefined — so every language server
      # (json/jsonc/json5, css, html, eslint) exits 1 the moment it starts. Define
      # an ESM-scope `require` via `createRequire` so the prelude resolves.
      vscode-langservers-extracted = prev.vscode-langservers-extracted.overrideAttrs (old: {
        postInstall = (old.postInstall or "") + ''
          for main in "$out"/lib/node_modules/vscode-langservers-extracted/lib/*/node/*ServerMain.js; do
            sed -i '1i import { createRequire as __nixCreateRequire } from "module"; const require = __nixCreateRequire(import.meta.url);' "$main"
          done
        '';
      });
    };

    # Custom vim plugins live under `pkgs.custom.vimPlugins` rather than being
    # merged into `pkgs.vimPlugins`. The editor looks there first when it
    # resolves a plugin by name, so units keep referencing them by name
    # without polluting the upstream set.
    vim-plugins =
      final: prev:

      let
        fromInput = name: inputs.${name}.packages.${prev.stdenv.hostPlatform.system}.default;
      in

      {
        custom = (prev.custom or { }) // {
          vimPlugins = {
            "dotfiles-nvim" = prev.callPackage ./dotfiles.nvim { };
            "note-nvim" = prev.callPackage ./note.nvim { };
            "alternaut-nvim" = fromInput "alternaut-nvim";
            "deja-view-nvim" = fromInput "deja-view-nvim";
            "gutenberg-nvim" = fromInput "gutenberg-nvim";
            "navitron-nvim" = fromInput "navitron-nvim";
            "teleport-vim" = fromInput "teleport-vim";
          };
        };
      };
  };
in

overlays
