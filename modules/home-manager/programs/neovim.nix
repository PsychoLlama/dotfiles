{ lib, config, pkgs, ... }:

with lib;

{
  options.programs.editors = mkOption {
    type = types.attrsOf (types.submoduleWith {
      modules = [ ../../editor ];
      specialArgs = { inherit pkgs; };
    });

    default = { };
    description = ''
      A set of customized Neovim editors. Each value is exposed as an alias.
    '';
  };

  config = {
    programs.nushell.initExtra = concatStringsSep "\n" (mapAttrsToList
      (name: value: "alias ${escapeShellArg name} = ${value.neovim}/bin/nvim")
      config.programs.editors);

    home.shellAliases = mapAttrs (name: value: "${value.neovim}/bin/nvim")
      config.programs.editors;
  };
}
