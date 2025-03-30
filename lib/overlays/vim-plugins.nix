inputs: self: pkgs:

let
  # Non-standard vim plugins. Mostly my own.
  mapToVimPlugins = pkgs.lib.mapAttrs' (
    pluginName: plugin: {
      name = pkgs.lib.replaceStrings [ "." ] [ "-" ] pluginName;
      value = pkgs.vimUtils.buildVimPlugin {
        pname = pluginName;
        version = plugin.shortRev or "latest";
        src = plugin;
      };
    }
  );

  extraVimPlugins = mapToVimPlugins {
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
      "ext-nvim" = pkgs.callPackage ../../pkgs/ext.nvim { };
      "alternaut-nvim" = inputs.alternaut-nvim.packages.${pkgs.system}.default;
    };
}
