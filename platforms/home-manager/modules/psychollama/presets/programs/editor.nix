{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (config.programs.editor) neovim;
  cfg = config.psychollama.presets.programs.editor;
in

{
  options.psychollama.presets.programs.editor = {
    enable = lib.mkEnableOption "Configure editor as the one true editor";
  };

  config = lib.mkIf cfg.enable {
    home.sessionVariables = {
      MANPAGER = "${neovim}/bin/nvim -c 'Man!'";
    };

    programs.editor = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.unstable.neovim;
      psychollama.profiles.full.enable = true;
    };
  };
}
