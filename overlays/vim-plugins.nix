inputs: self: pkgs:

let
  # Non-standard vim plugins. Mostly my own.
  mapToVimPlugins = with pkgs.lib;
    mapAttrs (pluginName: plugin:
      pkgs.vimUtils.buildVimPluginFrom2Nix {
        pname = pluginName;
        version = plugin.shortRev or "latest";
        src = plugin;
      });

  extraVimPlugins = mapToVimPlugins {
    inherit (inputs) further-vim teleport-vim alternaut-vim navitron-nvim;
    unison-vim = "${inputs.unison-vim}/editor-support/vim";
    personal-vim-config = ../config/editor;
  };

in { vimPlugins = pkgs.vimPlugins // extraVimPlugins; }
