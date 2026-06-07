{ config, lib, ... }:

let
  inherit (lib) types;
  cfg = config.programs.nushell.abbreviations;
in

{
  options.programs.nushell.abbreviations = lib.mkOption {
    type = types.attrsOf types.str;
    description = ''
      Abbreviations expanded inline at the prompt. Unlike aliases, the
      expansion is committed to history, keeping substring search reliable.
    '';
    default = { };
    example = {
      gs = "git status";
      ll = "ls -l";
    };
  };

  config.programs.nushell.extraConfig =
    lib.mkIf (cfg != { })
      # nu
      ''
        $env.config.abbreviations = ($env.config.abbreviations | merge ${lib.hm.nushell.toNushell { } cfg})
      '';
}
