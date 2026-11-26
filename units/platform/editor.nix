# The editor is configured by modules of its own class. Home Manager builds
# it, so that is where they are handed over.
{ rhizome, units, ... }:

let
  t = rhizome.types;
  inherit (units.platform) home-manager nixos user;
in

rhizome.unit "editor" (editor: {
  # A module of the editor's own class.
  module = rhizome.channel {
    level = user.level;
    type = t.any;
    merge = rhizome.merge.entries;
  };

  # Modules ready for an editor evaluation.
  lib.modules = map nixos.lib.locate;

  unit = rhizome.component {
    uses.entries = editor.module;

    config =
      { entries }:
      [ (home-manager.module { programs.editor.imports = editor.lib.modules entries; }) ];
  };
})
