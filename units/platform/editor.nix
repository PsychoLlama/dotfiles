# The editor is configured by modules of its own class. Home Manager builds
# it, so that is where they are handed over.
{ rhizome, units, ... }:

let
  t = rhizome.types;
  inherit (units.platform) home-manager level nixos;
in

rec {
  module = rhizome.channel "editor.module" {
    level = level.user;
    type.input = t.any;
    merge = rhizome.merge.entries;
  };

  # Modules ready for an editor evaluation.
  modules = map nixos.locate;

  component = rhizome.component "editor.modules" {
    uses.entries = module;
    config = { entries }: [ (home-manager.module { programs.editor.imports = modules entries; }) ];
  };
}
