{ lib }:

let
  builtinClasses = import ./classes.nix;
in

/*
  The mount: a module that installs every plugin into the host's own
  fixpoint, keyed by the plugin's handle. A plugin's modules mount as a
  nested option tree beneath it, mirroring the directory layout:

    options."${dotfiles}".programs.git.enable

  There is no separate rhizome eval. The mount evaluates exactly once, on
  the top-level host — nixos when there is one, home-manager standalone,
  or any custom `evalModules` (the editor). Fragments for the host's own
  class merge inline; fragments for every other class accumulate in
  `rhizome.fragments.<class>` for a router to carry onward.

  A module has three write blocks, one per target:

    config.services.foo.enable = true;                # its own plugin
    modules.nixos.users.users.bob.shell = ...;        # a host class
    peers."${inputs.hosts}".machines.x.enable = true; # a peer plugin

  Rhizome modules receive exactly six arguments — `self`, `cfg`,
  `inputs`, `global`, `lib`, `pkgs` — and nothing from the host.
  `global` is fenced to the mounted plugins, so a module can never
  observe (and grow dependent on) the host platform it happens to be
  evaluated in.

  Type: { class : String, plugins : AttrSet Plugin } -> Module
*/
{
  class,
  plugins,
}:

let
  # Registering the same plugin under two bindings mounts it once. The
  # binding name is only ever used for error messages.
  #
  # Two *instantiations* of one plugin are a genuine conflict: they share
  # a mount point, so one set of inputs would silently win. Comparing
  # forces the inputs, which is why a lone binding skips the check — an
  # unsupplied required input must stay lazy until something reads it.
  # Inputs are raw values, so this compares by structure: sharing one
  # instance is the reliable spelling, and the error says so.
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

  pluginList = lib.pipe plugins [
    (lib.mapAttrsToList (binding: plugin: { inherit binding plugin; }))
    (lib.groupBy (entry: entry.plugin.__plugin.key))
    (lib.mapAttrsToList dedupe)
  ];

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

  # `modules.<block>` key -> `_class` tag, across built-ins and all
  # plugins. Every block names a real class: a module says which host it
  # is configuring, and a block for some other class becomes a fragment
  # for a router to carry (or a warning when nothing claims it).
  classMap = lib.foldl' mergeClasses builtinClasses pluginList;

  classTags = lib.unique (lib.attrValues classMap);

  pluginKeys = lib.map (entry: entry.plugin.__plugin.key) pluginList;

  # One entry per mounted option namespace: every module of every plugin,
  # plus each plugin's own node. `path` is where its options mount.
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

  # The fenced read surface: mounted plugins only, never the host's own
  # options. Reads work on every module that is *mounted* — enablement
  # gates effects, not visibility.
  global = lib.genAttrs pluginKeys (key: config.${key});

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

  # Every module gets an implicit `enable` (plugin nodes default on) that
  # gates its writes and module blocks — loading is never the cut.
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

  # A fragment for the host's own class merges into the live fixpoint as
  # config. It gets the host's args (like any class fragment gets its
  # class's args) but cannot declare options or extend imports — config
  # cannot grow the eval. Anything needing that belongs at the assembly
  # site.
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

  # Fragments for every other class are deferred modules: they cross into
  # a fresh eval with full module powers, tagged so importing one into
  # the wrong platform fails loudly.
  wrapFragment = entry: block: fragment: {
    _file = "${entry.file}#modules.${block}";
    _class = classMap.${block};
    imports = [ fragment ];
  };

  # `peers.<handle>` writes a peer's namespace. It lands in the same
  # fixpoint `config` does, but a separate block means the reach is
  # declared rather than incidental — and an unmounted handle can say so
  # instead of surfacing as a missing option.
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

  # A module's `config` block is its plugin's namespace — the mount point
  # is implied, never spelled. Reaching out is explicit: `modules.<class>`
  # for the host, `peers.<handle>` for a peer.
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

  # Each module mounts as a module of its own, so the stock merge
  # machinery reports option collisions — and their provenance — for us.
  mountFor = entry: {
    _file = entry.file;
    options = lib.setAttrByPath entry.path (optionsFor entry);
    config = contributionFor entry;
  };

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
