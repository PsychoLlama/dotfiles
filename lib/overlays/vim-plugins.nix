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
    "teleport.vim" = inputs.teleport-vim;
  };
in

{
  vimPlugins =
    pkgs.vimPlugins
    // extraVimPlugins
    // {
      "lab-nvim" = pkgs.callPackage ../../pkgs/lab.nvim { };
      "alternaut-nvim" = inputs.alternaut-nvim.packages.${pkgs.system}.default;
    };
}
