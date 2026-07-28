{ lib }:

let
  discover = import ./discover.nix { inherit lib; };
  buildNamespace = import ./namespace.nix { inherit lib; };
in

/*
  Construct a plugin: a collection of meta-modules plus an addressable
  root node.

  The returned value is the plugin's namespace tree (the shape of every
  discovered module) and is itself the plugin's handle — the one string
  form in the system. It names the whole plugin's mount point, so
  consumers configure it as ordinary validated config:

    config."${dotfiles.plugin}".presets.programs.git.enable = true;
    config."${dotfiles.plugin}".identity.email = "...";

  Inside the plugin the handle is implied: a module's own `config` block
  is already rooted at this namespace.

  Plugins are plain values. Consumers register them with a root guest
  (`module.roots.nixos { dotfiles = inputs.dotfiles.plugin; }`); the
  binding name is used only for error messages.

  Type: {
    src : Path,                    # directory scanned for *.mod.nix / mod.nix
    classes? : AttrSet,            # extra `modules.<block>` -> `_class` tags
    root? : Module | null,         # caller-facing options for the whole plugin
  } -> Plugin
*/
{
  src,
  classes ? { },
  root ? null,
}:

let
  modules = discover src;
  namespace = buildNamespace modules;

  reserved = lib.filter (mod: lib.hasPrefix "__" (lib.head mod.subpath)) modules;
in

if reserved != [ ] then
  throw "module.plugin: module names may not start with `__` (got `${lib.concatStringsSep "." (lib.head reserved).subpath}` in ${toString src})."
else if classes ? root then
  throw "module.plugin: the `root` module block is reserved — it aliases whichever class the plugin is mounted in (${toString src})."
else
  namespace
  // {
    __toString = _: toString src;

    __plugin = {
      inherit
        src
        classes
        root
        modules
        namespace
        ;
      key = toString src;
    };
  }
