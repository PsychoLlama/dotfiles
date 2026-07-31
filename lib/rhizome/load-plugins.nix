{ lib }:

/**
  The load-time half of a mount: which plugins are installed, which
  `modules.<block>` keys they understand, and one entry per option
  namespace they contribute. Nothing here reads the host fixpoint, so it
  all resolves before a single module is applied.

  Two record shapes come out of here. A `Binding` is a plugin under the
  name the assembler registered it as; an `Entry` is one mounted option
  namespace — a module, or a plugin's own node:

  ```
  Binding = { binding : String, plugin : Plugin }

  Entry = Binding // {
    path : [String],        # where its options mount, key first
    file : String,          # `_file` for module-system provenance
    loader : Module,        # the unapplied module
    description : String,   # human-facing name, for errors
    isNode : Bool,          # plugin node, rather than a module
  }
  ```

  # Type

  ```
  loadPlugins :: AttrSet Plugin -> {
    entries : [ Entry ],
    classMap : AttrSet String,
    classTags : [ String ],
    pluginKeys : [ String ],
  }
  ```
*/

plugins:

let
  /**
    Collapse every binding that names one plugin into a single mount.
    Registering the same plugin under two bindings mounts it once; the
    binding name is only ever used for error messages.

    Two *instantiations* of one plugin are a genuine conflict: they share
    a mount point, so one set of inputs would silently win. Comparing
    forces the inputs, which is why a lone binding skips the check — an
    unsupplied required input must stay lazy until something reads it.
    Inputs are raw values, so this compares by structure: sharing one
    instance is the reliable spelling, and the error says so.

    # Type

    ```
    dedupe :: String -> [ Binding ] -> Binding
    ```
  */
  dedupe =
    key: group:
    let
      inputs = lib.map (entry: entry.plugin.__plugin.inputs) group;
      bindings = lib.concatStringsSep ", " (lib.map (entry: entry.binding) group);
    in
    if lib.length group == 1 || lib.all (given: given == lib.head inputs) inputs then
      lib.head group
    else
      throw "rhizome: plugin '${key}' is mounted more than once with different inputs (bindings: ${bindings}). Instantiate it once and share the result.";

  /**
    The mounted plugins, one entry each, keyed off the handle rather than
    the binding.

    # Type

    ```
    pluginList :: [ Binding ]
    ```
  */
  pluginList = lib.pipe plugins [
    (lib.mapAttrsToList (binding: plugin: { inherit binding plugin; }))
    (lib.groupBy (entry: entry.plugin.__plugin.key))
    (lib.mapAttrsToList dedupe)
  ];

  /**
    Fold one plugin's extra `modules.<block>` keys into the class table.
    Adding a block is how a plugin declares a new class; remapping one
    that is already spoken for is not, since a module's block would then
    mean different things depending on who else is mounted.

    # Type

    ```
    mergeClasses :: AttrSet String -> Binding -> AttrSet String
    ```
  */
  mergeClasses =
    acc: entry:
    let
      conflicts = lib.filterAttrs (
        block: tag: acc ? ${block} && acc.${block} != tag
      ) entry.plugin.__plugin.classes;
    in
    if conflicts == { } then
      acc // entry.plugin.__plugin.classes
    else
      throw "rhizome: plugin '${entry.binding}' remaps module block `${lib.head (lib.attrNames conflicts)}` to a different class tag.";

  /**
    `modules.<block>` key -> `_class` tag, seeded with the blocks every
    mount understands and extended by each plugin's own. Every block
    names a real class: a module says which host it is configuring, and
    a block for some other class becomes a fragment for a router to
    carry (or a failure when nothing claims it).

    # Type

    ```
    classMap :: AttrSet String
    ```
  */
  classMap = lib.foldl' mergeClasses {
    nixos = "nixos";
    darwin = "darwin";
    home-manager = "homeManager";
  } pluginList;

  /**
    Every class reachable from this mount, deduplicated: several blocks
    may tag the same class. Routers claim tags, not blocks.

    # Type

    ```
    classTags :: [ String ]
    ```
  */
  classTags = lib.unique (lib.attrValues classMap);

  /**
    The mounted handles. Doubles as the fence for `global` and the guest
    list `peers` writes are checked against.

    # Type

    ```
    pluginKeys :: [ String ]
    ```
  */
  pluginKeys = lib.map (entry: entry.plugin.__plugin.key) pluginList;

  /**
    One entry per mounted option namespace: every module of every plugin,
    plus each plugin's own node. Modules are loaded here — `import`ed but
    not applied — because loading is never the cut; `enable` is.

    # Type

    ```
    entries :: [ Entry ]
    ```
  */
  entries = lib.concatMap (
    { binding, plugin }:
    lib.map (mod: {
      inherit binding plugin;
      path = [ plugin.__plugin.key ] ++ mod.subpath;
      file = lib.toString mod.file;
      loader = import mod.file;
      description = "${binding}.${lib.concatStringsSep "." mod.subpath}";
      isNode = false;
    }) plugin.__plugin.modules
    ++ lib.optional (plugin.__plugin.node != null) {
      inherit binding plugin;
      path = [ plugin.__plugin.key ];
      file = plugin.__plugin.key;
      loader = plugin.__plugin.node;
      description = "${binding} (plugin node)";
      isNode = true;
    }
  ) pluginList;
in

{
  inherit
    entries
    classMap
    classTags
    pluginKeys
    ;
}
