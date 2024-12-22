inputs: self: pkgs:

let
  # Non-standard vim plugins. Mostly my own.
  mapToVimPlugins =
    with pkgs.lib;
    mapAttrs' (
      pluginName: plugin: {
        name = replaceStrings [ "." ] [ "-" ] pluginName;
        value = pkgs.vimUtils.buildVimPlugin {
          pname = pluginName;
          version = plugin.shortRev or "latest";
          src = plugin;
        };
      }

    );

  extraVimPlugins = mapToVimPlugins {
    "alternaut.nvim" = inputs.alternaut-vim;
    "codecompanion.nvim" = inputs.codecompanion-nvim;
    "deja-view.vim" = inputs.deja-view-vim;
    "navitron.nvim" = inputs.navitron-nvim;
    "remix.nvim" = inputs.tree-sitter-remix.packages.${pkgs.system}.remix-nvim;
    "teleport.vim" = inputs.teleport-vim;
  };
in
{
  vimPlugins =
    pkgs.vimPlugins
    // extraVimPlugins
    // {
      "ext-nvim" = pkgs.callPackage ../../packages/ext.nvim { };
    };
}
