{
  # Third-party packages built from source.
  flake.overlays.custom-packages = final: prev: {
    # Custom packages live under their own namespace so they never shadow (or get
    # shadowed by) upstream attributes on `pkgs.*`/`pkgs.unstable.*`.
    custom = (prev.custom or { }) // {
      chrome-devtools-mcp = final.callPackage ../../../pkgs/chrome-devtools-mcp { };
      claude-code-bin = final.callPackage ../../../pkgs/claude-code { };
      codex-bin = final.callPackage ../../../pkgs/codex { };
      nvim-rs = final.wrapNeovim (final.callPackage ../../../pkgs/nvim-rs { }) { };
    };
  };
}
