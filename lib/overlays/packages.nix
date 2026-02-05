# Third-party packages built from source.
_: final: prev: {
  chrome-devtools-mcp = final.callPackage ../../pkgs/chrome-devtools-mcp { };
  claude-code-bin = final.callPackage ../../pkgs/claude-code-bin { };
  codex = final.callPackage ../../pkgs/codex { };
}
