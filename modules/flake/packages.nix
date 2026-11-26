{
  inputs,
  units,
  ...
}:

let
  inherit (units) editor;
  inherit (units.platform) host user;

  # The editor's units, evaluated with no machine around them.
  standalone = inputs.rhizome.lib.eval {
    levels = [
      host.level
      user.level
    ];
  } editor.profiles.full.unit;
in

{
  perSystem =
    { pkgs, ... }:
    {
      packages = {
        editor = editor.lib.build pkgs {
          neovim = standalone.collect editor.neovim;
          plugins = standalone.collect editor.plugin;
          servers = standalone.collect editor.server;
          config = standalone.collect editor.config;
          tools = standalone.collect editor.tool;
        };

        inherit (pkgs.custom) chrome-devtools-mcp claude-code-bin codex-bin;
      };
    };
}
