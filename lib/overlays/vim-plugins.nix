inputs: final: prev:

let
  # Non-standard vim plugins. Mostly my own.
  mapToVimPlugins = prev.lib.mapAttrs' (
    pluginName: plugin: {
      name = prev.lib.replaceStrings [ "." ] [ "-" ] pluginName;
      value = prev.vimUtils.buildVimPlugin {
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
    prev.vimPlugins
    // extraVimPlugins
    // {
      "lab-nvim" = prev.callPackage ../../pkgs/lab.nvim { };
      "alternaut-nvim" = inputs.alternaut-nvim.packages.${prev.stdenv.hostPlatform.system}.default;
    };
}
