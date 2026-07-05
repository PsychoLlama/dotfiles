{
  lib,
  config,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.programs.claude-code.plugins.lua-lsp;

  luals = pkgs.unstable.lua-language-server;

  # Prints Neovim's runtime path to stdout, then exits (nvim's `-l` runs a script
  # and quits on its own). `io.write` keeps stdout clean; `print` would route
  # through the message system to stderr.
  print-runtime-script =
    pkgs.writeText "print-runtime.lua"
      # lua
      "io.write(vim.env.VIMRUNTIME)";

  # `.luarc.json` points its library at `$VIMRUNTIME/lua` for Neovim's Lua type
  # stubs. Inside the editor that variable is set; when Claude launches the
  # server standalone it isn't, so every `vim.*` reference reports
  # `undefined-global`. Detect an ambient `nvim`, borrow its runtime, and export
  # it before handing off. If nvim is absent (or already resolved), fall through
  # unchanged — the server still runs, just without Neovim's stubs.
  #
  # `</dev/null` isolates nvim from the wrapper's stdin, which is the LSP's
  # JSON-RPC stream — a stray read there would corrupt the protocol.
  server = pkgs.writeShellApplication {
    name = "lua-language-server-nvim";
    runtimeInputs = [ luals ];
    text = ''
      if [ -z "''${VIMRUNTIME:-}" ] && command -v nvim >/dev/null 2>&1; then
        VIMRUNTIME="$(nvim --clean --headless -l ${print-runtime-script} </dev/null)"
        export VIMRUNTIME
      fi

      exec lua-language-server "$@"
    '';
  };
in

{
  options.psychollama.presets.programs.claude-code.plugins.lua-lsp = {
    enable = lib.mkEnableOption "Lua LSP for Claude Code" // {
      default = true;
    };
  };

  config = lib.mkIf cfg.enable {
    programs.claude-code.localPlugins.lua-lsp = {
      description = "Lua Language Server for Claude Code.";

      lsp.servers.lua = {
        command = lib.getExe server;
        extensionToLanguage.".lua" = "lua";
      };
    };
  };
}
