inputs: self: pkgs:

let
  # Non-standard vim plugins. Mostly my own.
  mapToVimPlugins =
    with pkgs.lib;
    mapAttrs (
      pluginName: plugin:
      pkgs.vimUtils.buildVimPlugin {
        pname = pluginName;
        version = plugin.shortRev or "latest";
        src = plugin;
      }
    );

  extraVimPlugins = mapToVimPlugins {
    inherit (inputs)
      teleport-vim
      alternaut-vim
      navitron-nvim
      deja-view-vim
      ;
    inherit (inputs.tree-sitter-remix.packages.${pkgs.system}) remix-nvim;
    personal-vim-config = ../../config/editor;
  };
in
{
  vimPlugins = pkgs.vimPlugins // extraVimPlugins;
}
