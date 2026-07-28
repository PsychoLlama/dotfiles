import ../vim-plugin.nix "snacks-nvim" {
  extraConfig = ./config.lua;

  opts = {
    input.enabled = true;
    notifier.enabled = true;
  };
}
