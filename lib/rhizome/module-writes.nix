{ lib }:

let
  applyModule = import ./apply-module.nix { inherit lib; };
in

/**
  What one module writes, and where each write lands. A module's `config`
  block is its plugin's namespace — the mount point is implied, never
  spelled. Reaching out is explicit: `modules.<class>` for the host,
  `plugins.<handle>` for another plugin.

  Everything is gated on the module's `enable`, so a mounted module that
  nobody turned on contributes nothing.

  `Entry` is `load-plugins.nix`'s record, with the module already
  applied.

  # Type

  ```
  moduleWrites ::
    { class : String, classMap : AttrSet String, pluginKeys : [String] }
    -> { config : AttrSet, options : AttrSet, pkgs : AttrSet }
    -> Entry // { applied : AttrSet }
    -> AttrSet
  ```
*/

{
  class,
  classMap,
  pluginKeys,
}:

{
  config,
  options,
  pkgs,
}:

let
  /**
    Sort a module's `modules.<block>` writes by where they have to land:
    `inline` for this mount's own class, which merges into the live
    fixpoint, and `foreign` for every other class, which travels as a
    deferred fragment.

    # Type

    ```
    splitBlocks :: Entry -> { inline : AttrSet, foreign : AttrSet }
    ```
  */
  splitBlocks =
    entry:
    let
      blocks = entry.applied.modules or { };
      unknown = lib.attrNames (lib.removeAttrs blocks (lib.attrNames classMap));
    in
    if unknown == [ ] then
      {
        inline = lib.filterAttrs (block: _: classMap.${block} == class) blocks;
        foreign = lib.filterAttrs (block: _: classMap.${block} != class) blocks;
      }
    else
      throw "rhizome: ${entry.description} targets unknown module block `${lib.head unknown}`. Known blocks: ${lib.concatStringsSep ", " (lib.attrNames classMap)}.";

  /**
    Merge a fragment for the host's own class into the live fixpoint as
    config. It gets the host's args (like any class fragment gets its
    class's args) but cannot declare options or extend imports — config
    cannot grow the eval. Anything needing that belongs at the assembly
    site.

    # Type

    ```
    inlineFragment :: Entry -> String -> Module -> AttrSet
    ```
  */
  inlineFragment =
    entry: block: fragment:
    let
      applied = applyModule {
        description = "${entry.description}'s `modules.${block}`";
        subject = "Host-class fragments";
        available = {
          inherit
            config
            options
            pkgs
            lib
            ;
        };
      } fragment;

      body = if applied ? config then applied.config else applied;

      # Mounted plugins share this fixpoint, so a class block *could*
      # reach one. That reach is `plugins`' job — keeping it out of here
      # leaves `modules.<class>` meaning one thing: the host.
      reached = lib.filter (key: lib.elem key pluginKeys) (lib.attrNames body);
    in
    if applied ? options || applied ? imports then
      throw "rhizome: ${entry.description}'s `modules.${block}` runs in the live `${class}` fixpoint and cannot declare `options` or `imports`. Move those to the assembly site."
    else if reached != [ ] then
      throw "rhizome: ${entry.description}'s `modules.${block}` writes the plugin mounted at `${lib.head reached}`. Other plugins go through `plugins`, not a class block."
    else
      body;

  /**
    Package a fragment for another class as a deferred module: it crosses
    into a fresh eval with full module powers, tagged so importing one
    into the wrong platform fails loudly, and carrying the source file so
    the far side's errors still point back here.

    # Type

    ```
    wrapFragment :: Entry -> String -> Module -> Module
    ```
  */
  wrapFragment = entry: block: fragment: {
    _file = "${entry.file}#modules.${block}";
    _class = classMap.${block};
    imports = [ fragment ];
  };

  /**
    A module's writes into other plugins' namespaces, one attrset per
    plugin. They land in the same fixpoint `config` does, but a separate
    block means the reach is declared rather than incidental — and an
    unmounted handle can say so instead of surfacing as a missing option.

    # Type

    ```
    pluginWritesFor :: Entry -> [ AttrSet ]
    ```
  */
  pluginWritesFor =
    entry:
    let
      writes = entry.applied.plugins or { };
      unknown = lib.filter (key: !(lib.elem key pluginKeys)) (lib.attrNames writes);
    in
    if unknown == [ ] then
      lib.mapAttrsToList (key: body: { ${key} = body; }) writes
    else
      throw "rhizome: ${entry.description} writes the plugin at `${lib.head unknown}`, which is not mounted. Register it alongside '${entry.binding}'.";
in

entry:

let
  split = splitBlocks entry;
in

lib.mkIf (lib.getAttrFromPath (entry.path ++ [ "enable" ]) config) (
  lib.mkMerge (
    [ { ${lib.head entry.path} = entry.applied.config or { }; } ]
    ++ pluginWritesFor entry
    ++ lib.mapAttrsToList (block: fragment: inlineFragment entry block fragment) split.inline
    ++ lib.mapAttrsToList (block: fragment: {
      rhizome.fragments.${classMap.${block}} = [ (wrapFragment entry block fragment) ];
    }) split.foreign
  )
)
