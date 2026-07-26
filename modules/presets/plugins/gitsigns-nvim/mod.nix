import ../vim-plugin.nix "gitsigns-nvim" {
  extraConfig = ./config.lua;

  # Defer gitsigns until a buffer is read (~18ms saved)
  defer.event = "BufReadPre";
}
