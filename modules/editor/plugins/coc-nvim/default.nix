{ lib, config, ... }:

with lib;

{
  imports = [ ./settings.nix ./efm.nix ];

  config.withNodeJs = mkIf config.plugins.coc-nvim.enable (mkDefault true);
}
