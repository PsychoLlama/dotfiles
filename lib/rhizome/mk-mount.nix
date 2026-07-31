{ lib }:

/**
  The mount: a module that installs every plugin into the host's own
  fixpoint, keyed by the plugin's handle. A plugin's modules mount as a
  nested option tree beneath it, mirroring the directory layout:

  ```nix
  options."${dotfiles}".programs.git.enable
  ```

  There is no separate rhizome eval. The mount evaluates exactly once, on
  the top-level host — nixos when there is one, home-manager standalone,
  or any custom `evalModules` (the editor). Fragments for the host's own
  class merge inline; fragments for every other class accumulate in
  `rhizome.fragments.<class>` for a router to carry onward.

  A module has three write blocks, one per target:

  ```nix
  config.services.foo.enable = true;                # its own plugin
  modules.nixos.users.users.bob.shell = ...;        # a host class
  peers."${inputs.hosts}".machines.x.enable = true; # a peer plugin
  ```

  Rhizome modules receive exactly six arguments — `self`, `cfg`,
  `inputs`, `global`, `lib`, `pkgs` — and nothing from the host.
  `global` is fenced to the mounted plugins, so a module can never
  observe (and grow dependent on) the host platform it happens to be
  evaluated in.

  Two record shapes recur below. A `Binding` is a plugin under the name
  the assembler registered it as; an `Entry` is one mounted option
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
  mkMount :: { class : String, plugins : AttrSet Plugin } -> Module
  ```
*/
{
  class,
  plugins,
}:

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
  config,
  options,
  pkgs,
  ...
}:

let
  /**
    Apply a module to the arguments it asked for. Rhizome modules take a
    fixed, closed set — unlike the host module system, an unrecognised
    argument is an error rather than a lazy `_module.args` lookup, so a
    typo names itself instead of surfacing later as a missing option.

    # Type

    ```
    applyModule :: String -> AttrSet -> Module -> AttrSet
    ```
  */
  applyModule =
    description: available: loader:
    if !lib.isFunction loader then
      loader
    else
      let
        unknown = lib.attrNames (lib.removeAttrs (lib.functionArgs loader) (lib.attrNames available));
      in
      if unknown == [ ] then
        loader (lib.intersectAttrs (lib.functionArgs loader) available)
      else
        throw "rhizome: ${description} requested unavailable argument(s): ${lib.concatStringsSep ", " unknown}. Modules receive only: ${lib.concatStringsSep ", " (lib.attrNames available)}.";

  /**
    The fenced read surface: mounted plugins only, never the host's own
    options. Reads work on every module that is *mounted* — enablement
    gates effects, not visibility.

    # Type

    ```
    global :: AttrSet
    ```
  */
  global = lib.genAttrs pluginKeys (key: config.${key});

  /**
    Every entry with its module applied, ready to contribute options and
    config. `applied` is the module's own attrset: `options`, `config`,
    `modules`, `peers`.

    # Type

    ```
    evaluated :: [ Entry // { applied : AttrSet } ]
    ```
  */
  evaluated = lib.map (
    entry:
    entry
    // {
      # `self` is the plugin's own config tree and `cfg` this module's
      # slice of it — both ordinary lazy reads, so navigating past the
      # first hop is strict and loud on typos. `inputs` is load-time
      # data, the only thing here safe to use as an attribute name.
      applied = applyModule entry.description {
        self = config.${lib.head entry.path};
        cfg = lib.getAttrFromPath entry.path config;
        inputs = entry.plugin.__plugin.inputs;
        inherit global lib pkgs;
      } entry.loader;
    }
  ) entries;

  /**
    A module's declared options, plus the implicit `enable` that gates
    its writes — loading is never the cut. A module may declare `enable`
    itself to change its type or default; a plugin node defaults on,
    since the plugin is already opt-in by being mounted.

    # Type

    ```
    optionsFor :: Entry -> AttrSet
    ```
  */
  optionsFor =
    entry:
    let
      declared = entry.applied.options or { };
    in
    declared
    // lib.optionalAttrs (!(declared ? enable)) {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = entry.isNode;
        description = "Whether to activate `${entry.description}`.";
      };
    };

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
      hostArgs = {
        inherit
          config
          options
          pkgs
          lib
          ;
      };
      applied =
        if !lib.isFunction fragment then
          fragment
        else
          let
            unknown = lib.attrNames (lib.removeAttrs (lib.functionArgs fragment) (lib.attrNames hostArgs));
          in
          if unknown == [ ] then
            fragment (lib.intersectAttrs (lib.functionArgs fragment) hostArgs)
          else
            throw "rhizome: ${entry.description}'s `modules.${block}` requested unavailable argument(s): ${lib.concatStringsSep ", " unknown}. Host-class fragments receive only: ${lib.concatStringsSep ", " (lib.attrNames hostArgs)}.";

      body = if applied ? config then applied.config else applied;

      # Mounted plugins share this fixpoint, so a class block *could*
      # reach one. That reach is `peers`' job — keeping it out of here
      # leaves `modules.<class>` meaning one thing: the host.
      reached = lib.filter (key: lib.elem key pluginKeys) (lib.attrNames body);
    in
    if applied ? options || applied ? imports then
      throw "rhizome: ${entry.description}'s `modules.${block}` runs in the live `${class}` fixpoint and cannot declare `options` or `imports`. Move those to the assembly site."
    else if reached != [ ] then
      throw "rhizome: ${entry.description}'s `modules.${block}` writes the plugin mounted at `${lib.head reached}`. Peer plugins go through `peers`, not a class block."
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
    A module's writes into its peers' namespaces, one attrset per peer.
    They land in the same fixpoint `config` does, but a separate block
    means the reach is declared rather than incidental — and an unmounted
    handle can say so instead of surfacing as a missing option.

    # Type

    ```
    peerWritesFor :: Entry -> [ AttrSet ]
    ```
  */
  peerWritesFor =
    entry:
    let
      writes = entry.applied.peers or { };
      unknown = lib.filter (key: !(lib.elem key pluginKeys)) (lib.attrNames writes);
    in
    if unknown == [ ] then
      lib.mapAttrsToList (key: body: { ${key} = body; }) writes
    else
      throw "rhizome: ${entry.description} writes the plugin at `${lib.head unknown}`, which is not mounted. Register it alongside '${entry.binding}'.";

  /**
    Everything one module contributes to the fixpoint, gated on its
    `enable`: its own config, its peer writes, its inline class blocks,
    and its foreign ones parked in `rhizome.fragments`.

    A module's `config` block is its plugin's namespace — the mount point
    is implied, never spelled. Reaching out is explicit: `modules.<class>`
    for the host, `peers.<handle>` for a peer.

    # Type

    ```
    contributionFor :: Entry -> AttrSet
    ```
  */
  contributionFor =
    entry:
    let
      split = splitBlocks entry;
    in
    lib.mkIf (lib.getAttrFromPath (entry.path ++ [ "enable" ]) config) (
      lib.mkMerge (
        [ { ${lib.head entry.path} = entry.applied.config or { }; } ]
        ++ peerWritesFor entry
        ++ lib.mapAttrsToList (block: fragment: inlineFragment entry block fragment) split.inline
        ++ lib.mapAttrsToList (block: fragment: {
          rhizome.fragments.${classMap.${block}} = [ (wrapFragment entry block fragment) ];
        }) split.foreign
      )
    );

  /**
    One rhizome module as one host module. Keeping them separate is what
    makes the stock merge machinery report option collisions — and their
    provenance — for us.

    # Type

    ```
    mountFor :: Entry -> Module
    ```
  */
  mountFor = entry: {
    _file = entry.file;
    options = lib.setAttrByPath entry.path (optionsFor entry);
    config = contributionFor entry;
  };

  /**
    The mount's own options: where foreign fragments collect, who claimed
    them, and what nobody did. Declared once for the whole mount rather
    than per module.

    # Type

    ```
    bookkeeping :: Module
    ```
  */
  bookkeeping = {
    options.rhizome = {
      fragments = lib.mkOption {
        type = lib.types.attrsOf (lib.types.listOf lib.types.deferredModule);
        description = ''
          Deferred class fragments per class tag, contributed by enabled
          modules. Routers carry each class into its target eval (e.g.
          `home-manager.sharedModules`).
        '';
      };

      routed = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Class tags claimed by a router.";
      };

      dropped = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = ''
          Class tags whose fragments are deliberately discarded. A root
          declares the classes that can never apply to its stack — a nixos
          host has no use for darwin fragments — so that whatever is left
          over reads as an oversight rather than a choice.
        '';
      };

      unrouted = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        readOnly = true;
        description = ''
          Class tags holding fragments that no router claimed and no root
          discarded: configuration that was written and then went nowhere.
        '';
        default =
          let
            claimed = config.rhizome.routed ++ config.rhizome.dropped;

            # A typo here would silently fail to claim, which is the exact
            # silence the assertion downstream exists to remove.
            unknown = lib.subtractLists classTags claimed;
          in
          if unknown != [ ] then
            throw "rhizome: `rhizome.routed`/`rhizome.dropped` names unknown class tag(s): ${lib.concatStringsSep ", " unknown}. Known tags: ${lib.concatStringsSep ", " classTags}."
          else
            lib.pipe config.rhizome.fragments [
              (lib.filterAttrs (tag: fragments: fragments != [ ] && !(lib.elem tag claimed)))
              lib.attrNames
            ];
      };
    };

    config.rhizome.fragments = lib.genAttrs classTags (_: [ ]);
  };
in

{
  imports = lib.map mountFor evaluated ++ [ bookkeeping ];
}
