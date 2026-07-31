{ lib }:

let
  discover = import ./discover.nix { inherit lib; };
  buildNamespace = import ./namespace.nix { inherit lib; };
in

/**
  Define a plugin: a collection of rhizome modules plus a module of its
  own, mounted at the plugin's root. Definition and instantiation are separate calls — this
  returns a function, and applying it to an input set yields the
  mountable plugin.

  The instance is the plugin's namespace tree (the shape of every
  discovered module) and is itself the plugin's handle — the one string
  form in the system. It names the whole plugin's mount point, so
  consumers configure it as ordinary validated config:

  ```nix
  config."${dotfiles}".programs.git.enable = true;
  config."${dotfiles}".identity.email = "...";
  ```

  Inside the plugin the handle is implied: a module's own `config` block
  is already rooted at this namespace.

  `inputs` declares the values a plugin expects from whoever assembles
  the mount, as an attrset of defaults. Every module in the plugin
  receives them as an `inputs` argument, verbatim — usually a reference
  to another plugin, but any value works, and none of them are inspected
  or transformed. Required inputs default to `throw`; optional ones to
  `null`, for the plugin to detect:

  ```nix
  inputs = {
    hosts = throw "dotfiles: input `hosts` is required.";
    secrets = null;
  };
  ```

  A plugin arrives as a plugin, so a module names another one the same
  way a consumer does — by interpolating it into the `plugins` block:

  ```nix
  plugins."${inputs.hosts}".machines.laptop.enable = true;
  ```

  Inputs are load-time by construction: their job is to appear in
  attribute-name position, which options cannot do without forcing the
  option tree during its own assembly. Only the assembler supplies them
  — a plugin never instantiates another.

  Plugins are plain values. Consumers instantiate, then register them
  with a mount:

  ```nix
  let
    hosts = inputs.hosts.plugin { };
    dotfiles = inputs.dotfiles.plugin { inherit hosts; };
  in
  rhizome.mounts.nixos { inherit dotfiles hosts; }
  ```

  The binding name is used only for error messages.

  # Type

  ```
  plugin :: {
    src : Path,                    # directory scanned for *.mod.nix / mod.nix
    classes? : AttrSet,            # extra `modules.<block>` -> `_class` tags
    inputs? : AttrSet,             # expected inputs -> default values
    configure? : Module | null,    # the module at the plugin's own root
  } -> AttrSet -> Plugin
  ```
*/
{
  src,
  classes ? { },
  inputs ? { },
  configure ? null,
}:

let
  modules = discover src;
  namespace = buildNamespace modules;

  reserved = lib.filter (mod: lib.hasPrefix "__" (lib.head mod.subpath)) modules;
in

supplied:

let
  unknown = lib.attrNames (lib.removeAttrs supplied (lib.attrNames inputs));
in

if reserved != [ ] then
  throw "rhizome: module names may not start with `__` (got `${lib.concatStringsSep "." (lib.head reserved).subpath}` in ${lib.toString src})."
else if unknown != [ ] then
  throw "rhizome: ${lib.toString src} was given unexpected input(s): ${lib.concatStringsSep ", " unknown}. It declares: ${lib.concatStringsSep ", " (lib.attrNames inputs)}."
else
  namespace
  // {
    __toString = _: lib.toString src;

    __plugin = {
      inherit classes configure modules;
      key = lib.toString src;
      inputs = inputs // supplied;
    };
  }
