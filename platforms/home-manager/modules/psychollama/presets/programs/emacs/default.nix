{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.programs.emacs;
in

{
  options.psychollama.presets.programs.emacs = {
    enable = lib.mkEnableOption "Use an opinionated Emacs config";
  };

  config.programs.emacs = lib.mkIf cfg.enable {
    enable = true;
    package = lib.mkDefault pkgs.unstable.emacs;
    config-file = ./emacs.el;
    extraPackages = plugins: [
      # Major modes
      plugins.lua-mode
      plugins.markdown-mode
      plugins.nix-mode

      # Core features
      plugins.apheleia
      plugins.atom-one-dark-theme
      plugins.company
      plugins.copilot
      plugins.counsel
      plugins.eglot
      plugins.evil
      plugins.evil-collection
      plugins.evil-commentary
      plugins.evil-surround
      plugins.evil-terminal-cursor-changer
      plugins.magit
      plugins.paredit
      plugins.projectile
      plugins.tree-sitter-langs
      plugins.treesit-grammars.with-all-grammars
      plugins.undo-tree
      plugins.xclip
    ];
  };
}
