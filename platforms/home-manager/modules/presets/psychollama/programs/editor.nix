{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (config.programs.editor) neovim;
  cfg = config.presets.programs.editor;
in

{
  options.presets.programs.editor = {
    enable = lib.mkEnableOption "Configure editor as the one true editor";
  };

  config = lib.mkIf cfg.enable {
    home.sessionVariables = {
      EDITOR = "${neovim}/bin/nvim";
      MANPAGER = "${neovim}/bin/nvim -c 'Man!'";
    };

    programs.editor = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.unstable.neovim;
      profiles.full.enable = true;
    };
  };
}
