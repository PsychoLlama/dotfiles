{ lib }:

let
  discover = import ./discover.nix { inherit lib; };
  buildNamespace = import ./namespace.nix { inherit lib; };
in

/*
  Construct a plugin: a collection of meta-modules plus an addressable
  root node.

  The returned value is the plugin's namespace tree (handles for every
  discovered module) and is itself a handle — its string form mounts the
  `root` module, so plugin-level settings are ordinary validated config:

    config."${dotfiles.plugin}".identity.email = "...";

  Plugins are plain values. Consumers register them with a root guest
  (`module.roots.nixos { dotfiles = inputs.dotfiles.plugin; }`); the
  binding name is used only for error messages.

  Type: {
    src : Path,                    # directory scanned for *.mod.nix / mod.nix
    classes? : AttrSet,            # extra `platforms.<block>` -> `_class` tags
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
