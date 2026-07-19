# Third-party packages built from source, plus overrides for broken upstreams.
_: final: prev: {
  # Custom packages live under their own namespace so they never shadow (or get
  # shadowed by) upstream attributes on `pkgs.*`/`pkgs.unstable.*`.
  custom = (prev.custom or { }) // {
    chrome-devtools-mcp = final.callPackage ../../pkgs/chrome-devtools-mcp { };
    claude-code-bin = final.callPackage ../../pkgs/claude-code { };
    codex-bin = final.callPackage ../../pkgs/codex { };
    nvim-rs = final.wrapNeovim (final.callPackage ../../pkgs/nvim-rs { }) { };
  };

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
}
