import ../vim-plugin.nix "treesj" {
  extraConfig = ./config.lua;

  # Defer treesj until its keymap is used (~13ms saved)
  defer.keys = "<leader>j";
}
