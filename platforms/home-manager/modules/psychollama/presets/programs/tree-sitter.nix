{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.programs.tree-sitter;
  json = pkgs.formats.json { };
in

{
  options.psychollama.presets.programs.tree-sitter = {
    enable = lib.mkEnableOption "Pre-configure tree-sitter with all grammars";
    grammars = lib.mkOption {
      type = lib.types.package;
      default = pkgs.unstable.tree-sitter.grammars;
      description = "Set of tree-sitter grammars to install";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.tree-sitter = {
      enable = true;
      package = lib.mkDefault pkgs.unstable.tree-sitter;
    };

    xdg.configFile."tree-sitter/config.json".source = json.generate "tree-sitter-config.json" {
      parser-directories = [ cfg.grammars ];
    };
  };
}
