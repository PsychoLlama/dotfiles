{ lib, config, ... }:

with lib;

{
  imports = [ ./settings.nix ];

  config.withNodeJs = mkIf config.plugins.coc-nvim.enable (mkDefault true);
}
