{
  lib,
  config,
  pkgs,
  ...
}:

let
  # Simple preset: just a name and optional config file
  mkPluginPreset = name: extraConfig: mkPluginPresetFull name { inherit extraConfig; };

  # Full preset with all options including defer
  mkPluginPresetFull =
    name:
    {
      extraConfig ? null,
      defer ? null,
    }:

    let
      cfg = config.psychollama.presets.plugins.${name};
    in

    {
      # Install the latest version of a vim plugin.
      options.psychollama.presets.plugins.${name} = {
        enable = lib.mkEnableOption "Install vimPlugins.${name}";
        package = lib.mkPackageOption pkgs.unstable.vimPlugins name { };
      };

      config.plugins.${name} = lib.mkIf cfg.enable {
        enable = lib.mkDefault true;
        package = lib.mkDefault cfg.package;
        inherit extraConfig defer;
      };
    };
in

{
  imports = [
    (mkPluginPreset "alternaut-nvim" ./alternaut/config.lua)
    (mkPluginPreset "cmp-buffer" null)
    (mkPluginPreset "cmp-cmdline" null)
    (mkPluginPreset "cmp-nvim-lsp" null)
    (mkPluginPreset "cmp-path" null)

    # Defer codecompanion until its commands or keymap are used (~26ms saved)
    (mkPluginPresetFull "codecompanion-nvim" {
      extraConfig = ./codecompanion/config.lua;
      defer.cmd = [
        "CodeCompanion"
        "CodeCompanionChat"
        "CodeCompanionCmd"
        "CodeCompanionActions"
      ];
      defer.keys = "<leader>c";
    })
    (mkPluginPreset "conform-nvim" ./conform/config.lua)
    (mkPluginPreset "deja-view-vim" null)
    (mkPluginPreset "lab-nvim" null)
    (mkPluginPreset "fzf-vim" null)
    (mkPluginPreset "gitlinker-nvim" ./gitlinker.lua)

    # Defer gitsigns until a buffer is read (~18ms saved)
    (mkPluginPresetFull "gitsigns-nvim" {
      extraConfig = ./gitsigns.lua;
      defer.event = "BufReadPre";
    })

    (mkPluginPreset "lualine-lsp-progress" null)
    (mkPluginPreset "lualine-nvim" ./lualine.lua)

    # Defer markdown-preview until markdown files are opened
    (mkPluginPresetFull "markdown-preview-nvim" {
      extraConfig = ./markdown-preview/config.lua;
      defer.ft = "markdown";
    })

    (mkPluginPreset "navitron-nvim" ./navitron.lua)
    (mkPluginPreset "nvim-autopairs" ./autopairs.lua)
    (mkPluginPreset "nvim-cmp" ./nvim-cmp.lua)
    (mkPluginPreset "nvim-lint" ./nvim-lint/config.lua)
    (mkPluginPreset "nvim-luapad" null)
    (mkPluginPreset "nvim-treesitter" ./nvim-treesitter.lua)
    (mkPluginPreset "nvim-treesitter-textobjects" null)
    (mkPluginPreset "onedarkpro-nvim" ./onedarkpro.lua)
    (mkPluginPreset "snacks-nvim" ./snacks/config.lua)
    (mkPluginPreset "teleport-vim" ./teleport.lua)
    (mkPluginPreset "telescope-fzf-native-nvim" ./telescope-fzf-native.lua)
    (mkPluginPreset "telescope-nvim" ./telescope.lua)
    (mkPluginPreset "telescope-undo-nvim" ./telescope-undo.lua)

    # Defer treesj until its keymap is used (~13ms saved)
    (mkPluginPresetFull "treesj" {
      extraConfig = ./treesj.lua;
      defer.keys = "<leader>j";
    })

    (mkPluginPreset "vim-endwise" null)
    (mkPluginPreset "vim-fugitive" null)
    (mkPluginPreset "vim-repeat" null)
    (mkPluginPreset "vim-surround" null)
  ];
}
