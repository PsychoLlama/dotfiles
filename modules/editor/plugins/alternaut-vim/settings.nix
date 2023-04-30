{ lib, config, pkgs, ... }:

with lib;

let
  cfg = config.plugins.alternaut-vim;
  jsonFormat = pkgs.formats.json { };
  patternsFile = jsonFormat.generate "alternaut-patterns.json" cfg.patterns;

in {
  options.plugins.alternaut-vim.patterns = mkOption {
    type = jsonFormat.type;
    description = "File naming conventions and patterns";
    default = { };
  };

  config.plugins.alternaut-vim.extraConfig = mkIf (cfg.patterns != { }) ''
    let g:alternaut#conventions = json_decode(readfile("${patternsFile}"))
  '';
}
