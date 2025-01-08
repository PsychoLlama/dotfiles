{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkOption types;
  lua = lib.generators.toLua { };
  configFile = pkgs.writeText "user-config.lua" config.extraConfig;

  # This is the generated `&packpath` directory for all plugins.
  packdir = pkgs.vimUtils.packDir {
    managed-by-nix = {
      start = [
        # Core framework, automatically loaded. Provides package loading and
        # language server support.
        (pkgs.vimUtils.buildVimPlugin {
          pname = "neovim-core";
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
    ./settings.nix
  ];

  options = {
    enable = lib.mkEnableOption "Whether to enable Neovim";

    neovim = mkOption {
      type = types.package;
      description = "The generated neovim package";
      readOnly = true;
      default = config.package.override {
        inherit (config) withNodeJs;

        extraMakeWrapperArgs = lib.concatStringsSep " " [
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
          require('core.settings').apply(${lua config.settings})
          require('core.pkg._loader').set_manifest(${lua config.core.manifest})
          require('core.lsp').setup(${lua config.core.lsp.servers})
          CORE_FRAMEWORK

          source ${configFile}

          " Run per-plugin configs after the vimrc finishes.
          lua require('core.pkg._loader').eval_configs()
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
