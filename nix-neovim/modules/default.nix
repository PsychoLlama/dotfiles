{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  lua = lib.generators.toLua { };
  configFile = pkgs.writeText "user-config.lua" config.extraConfig;

  # This is the generated `&packpath` directory for all plugins.
  packdir = pkgs.vimUtils.packDir {
    managed-by-nix = {
      start = [
        # Core framework, automatically loaded. Provides package loading and
        # language server support.
        (pkgs.vimUtils.buildVimPlugin {
          pname = "nix-neovim";
          version = "latest";
          src = ../runtime;
        })
      ];

      # Provide access to all plugins but don't load them immediately.
      opt = config.core.packages;
    };
  };
in

{
  imports = [
    ./lsp
    ./plugins.nix
    ./presets
  ];

  options = {
    enable = mkEnableOption "Whether to enable Neovim";

    neovim = mkOption {
      type = types.package;
      description = "The generated neovim package";
      readOnly = true;
      default = config.package.override {
        inherit (config) withNodeJs;

        extraMakeWrapperArgs = concatStringsSep " " [
          "--suffix PATH : ${lib.makeBinPath config.extraPackages}"
          (lib.cli.toGNUCommandLineShell { } {
            add-flags = [
              ''--cmd "set packpath^=${packdir}"''
              ''--cmd "set rtp^=${packdir}"''
            ];
          })
        ];

        configure.customRC = ''
          lua << CORE_FRAMEWORK
          require('core.pkg._manifest').set(${lua config.core.manifest})
          require('core.lsp').setup(${lua config.core.lsp.servers})
          CORE_FRAMEWORK

          source ${configFile}
        '';
      };
    };

    package = mkOption {
      type = types.package;
      description = "A neovim editor";
      default = pkgs.neovim;
    };

    withNodeJs = mkOption {
      type = types.bool;
      description = "Whether to enable Node.js support";
      default = true;
    };

    extraPlugins = mkOption {
      type = types.listOf types.package;
      description = "Extra plugins to install";
      default = [ ];
    };

    extraPackages = mkOption {
      type = types.listOf types.package;
      description = "Extra packages visible to Neovim";
      default = [ ];
    };

    extraConfig = mkOption {
      type = types.lines;
      description = "Extra init.lua config";
      default = "";
    };
  };
}
