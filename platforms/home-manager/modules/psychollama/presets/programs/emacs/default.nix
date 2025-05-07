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

  config = lib.mkIf cfg.enable {
    home.shellAliases.e = "emacs -nw";

    programs.emacs = {
      enable = true;
      package = lib.mkDefault pkgs.unstable.emacs;
      extraConfig = builtins.readFile ./emacs.el;
      extraPackages = emacsPackages: [
        emacsPackages.atom-one-dark-theme
        emacsPackages.evil
        emacsPackages.magit
      ];
    };
  };
}
