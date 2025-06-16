{ lib, config, ... }:

let
  cfg = config.psychollama.presets.plugins.codecompanion-nvim;
in

{
  config.plugins.codecompanion-nvim = lib.mkIf cfg.enable {
    # Reference:
    # codecompanion.nvim/lua/codecompanion/config.lua
    opts.strategies.chat.roles = {
      llm = "Computer";
      user = "User";
    };
  };
}
