{
  lib,
  config,
  pkgs,
  ...
}:

let
  mkPluginPreset =
    name: extraConfig:

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
        inherit extraConfig;
      };
    };
in

{
  imports = [
    ./alternaut
    ./codecompanion
    ./conform
    ./markdown-preview
    ./nvim-lint
    ./snacks

    (mkPluginPreset "alternaut-nvim" ./alternaut/config.lua)
    (mkPluginPreset "cmp-buffer" null)
    (mkPluginPreset "cmp-cmdline" null)
    (mkPluginPreset "cmp-nvim-lsp" null)
    (mkPluginPreset "cmp-path" null)
    (mkPluginPreset "codecompanion-nvim" ./codecompanion/config.lua)
    (mkPluginPreset "conform-nvim" ./conform/config.lua)
    (mkPluginPreset "copilot-lua" ./copilot.lua)
    (mkPluginPreset "deja-view-vim" null)
    (mkPluginPreset "ext-nvim" null)
    (mkPluginPreset "fzf-vim" null)
    (mkPluginPreset "gitlinker-nvim" ./gitlinker.lua)
    (mkPluginPreset "gitsigns-nvim" ./gitsigns.lua)
    (mkPluginPreset "lualine-lsp-progress" null)
    (mkPluginPreset "lualine-nvim" ./lualine.lua)
    (mkPluginPreset "markdown-preview-nvim" ./markdown-preview/config.lua)
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
    (mkPluginPreset "treesj" ./treesj.lua)
    (mkPluginPreset "vim-endwise" null)
    (mkPluginPreset "vim-fugitive" null)
    (mkPluginPreset "vim-repeat" null)
    (mkPluginPreset "vim-surround" null)
  ];
}
