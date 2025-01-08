{ lib, pkgs, ... }:

let
  inherit (lib) types;
  json = pkgs.formats.json { };
in

{
  options.settings = lib.mkOption {
    default = { };
    type = types.attrsOf json.type;

    description = ''
      Neovim option name. See `:help option-list`.
      Replaces the default option.
    '';
  };
}
