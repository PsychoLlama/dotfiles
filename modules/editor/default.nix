{ config, lib, pkgs, ... }:

with lib;

{
  imports = [ ./plugins.nix ];

  options = {
    neovim = mkOption {
      type = types.package;
      description = "The generated neovim package";
      readOnly = true;
      default = config.package.override {
        configure = {
          packages.plugins.start = config.extraPlugins;
          customRC = config.extraConfig;
        };
      };
    };

    package = mkOption {
      type = types.package;
      description = "A neovim editor";
      default = pkgs.neovim;
    };

    extraPlugins = mkOption {
      type = types.listOf types.package;
      description = "Extra plugins to install";
      default = [];
    };

    extraConfig = mkOption {
      type = types.lines;
      description = "Extra vimrc config";
      default = "";
    };
  };
}
