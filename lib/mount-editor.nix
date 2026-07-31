{ self, ... }:

# Mounts rhizome plugins into a standalone editor.
#
# There is no outer host to route fragments down from, so the plugins mount
# at the editor itself. `class = "editor"` makes editor payloads merge
# inline; every other class is discarded on purpose — a portable editor has
# no OS or home to configure, which is the whole point of shipping it this
# way.
#
# The `editor` class belongs to the dotfiles plugin, not to rhizome, so this
# root lives here rather than beside `mounts.nixos`.
#
# Type: AttrSet Plugin -> Module

plugins:

self.lib.rhizome.mounts.custom {
  class = "editor";
  inherit plugins;

  configure.rhizome.dropped = [
    "nixos"
    "darwin"
    "homeManager"
  ];
}
