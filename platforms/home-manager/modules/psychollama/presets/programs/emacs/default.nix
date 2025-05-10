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
    extraConfig = builtins.readFile ./emacs.el;
    extraPackages = plugins: [
      plugins.atom-one-dark-theme
      plugins.company
      plugins.counsel
      plugins.eglot
      plugins.evil
      plugins.evil-collection
      plugins.evil-commentary
      plugins.evil-surround
      plugins.magit
      plugins.projectile
      plugins.tree-sitter-langs
      plugins.treesit-grammars.with-all-grammars
      plugins.evil-terminal-cursor-changer

      plugins.markdown-mode
      plugins.nix-mode
      plugins.lua-mode
    ];
  };
}
