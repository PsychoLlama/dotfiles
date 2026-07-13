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
    "teleport.vim" = inputs.teleport-vim;
  };
in

{
  # Custom vim plugins live under `pkgs.custom.vimPlugins` rather than being
  # merged into `pkgs.vimPlugins`. The editor platform merges them back into the
  # by-name lookup via its `vimPlugins` option, so presets keep referencing them
  # by name without polluting the upstream set.
  custom = (prev.custom or { }) // {
    vimPlugins = extraVimPlugins // {
      "lab-nvim" = prev.callPackage ../../pkgs/lab.nvim { };
      "note-nvim" = prev.callPackage ../../pkgs/note.nvim { };
      "alternaut-nvim" = inputs.alternaut-nvim.packages.${prev.stdenv.hostPlatform.system}.default;
      "deja-view-nvim" = inputs.deja-view-nvim.packages.${prev.stdenv.hostPlatform.system}.default;
      "gutenberg-nvim" = inputs.gutenberg-nvim.packages.${prev.stdenv.hostPlatform.system}.default;
      "navitron-nvim" = inputs.navitron-nvim.packages.${prev.stdenv.hostPlatform.system}.default;
    };
  };
}
