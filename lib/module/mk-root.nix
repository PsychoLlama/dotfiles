{ lib }:

let
  builtinClasses = import ./classes.nix;
in

/*
  The root guest: a module that mounts every plugin into the host root's
  own fixpoint, keyed by the plugin's handle. A plugin's modules mount as
  a nested option tree beneath it, mirroring the directory layout:

    options."${dotfiles}".presets.programs.git.enable

  There is no separate meta eval. The guest evaluates exactly once, on
  the top-level root — nixos when there is one, home-manager standalone,
  or any custom `evalModules` (the editor). Fragments for the root's own
  class merge inline; fragments for every other class accumulate in
  `_meta.fragments.<class>` for an installer to route onward.

  A module has three write blocks, one per target:

    config.services.foo.enable = true;                  # its own plugin
    modules.root.users.bob.shell = ...;                 # the host
    plugins."${inputs.hosts}".machines.x.enable = true; # a peer plugin

  Meta modules receive exactly six arguments — `self`, `cfg`, `inputs`,
  `global`, `lib`, `pkgs` — and nothing from the root. `global` is fenced
  to the mounted plugins, so a module can never observe (and grow
  dependent on) the host platform it happens to be evaluated in.

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
      inputs = map (entry: entry.plugin.__plugin.inputs) group;
      bindings = lib.concatStringsSep ", " (map (entry: entry.binding) group);
    in
    if lib.length group == 1 || lib.all (given: given == lib.head inputs) inputs then
      lib.head group
    else
      throw "module system: plugin '${key}' is mounted more than once with different inputs (bindings: ${bindings}). Instantiate it once and share the result.";

  pluginList = lib.mapAttrsToList dedupe (
    lib.groupBy (entry: entry.plugin.__plugin.key) (
      lib.mapAttrsToList (binding: plugin: { inherit binding plugin; }) plugins
    )
  );

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
      throw "module system: plugin '${entry.binding}' remaps module block `${lib.head (lib.attrNames conflicts)}` to a different class tag.";

  # `modules.<block>` key -> `_class` tag, across built-ins and all
  # plugins. `root` is an alias for whichever class this root evaluates
  # in, so a module can target its host without naming it — and so it can
  # reach a peer plugin's namespace, which lives in that same fixpoint.
  classMap = lib.foldl' mergeClasses builtinClasses pluginList // {
    root = class;
  };

  classTags = lib.unique (lib.attrValues classMap);

  pluginKeys = map (entry: entry.plugin.__plugin.key) pluginList;

  # One entry per mounted option namespace: every module of every plugin,
  # plus each plugin's root node. `path` is where its options mount.
  entries = lib.concatMap (
    { binding, plugin }:
    map (mod: {
      inherit binding plugin;
      path = [ plugin.__plugin.key ] ++ mod.subpath;
      file = toString mod.file;
      loader = import mod.file;
      description = "${binding}.${lib.concatStringsSep "." mod.subpath}";
      isRoot = false;
    }) plugin.__plugin.modules
    ++ lib.optional (plugin.__plugin.root != null) {
      inherit binding plugin;
      path = [ plugin.__plugin.key ];
      file = plugin.__plugin.key;
      loader = plugin.__plugin.root;
      description = "${binding} (root node)";
      isRoot = true;
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
        unknown = lib.attrNames (removeAttrs (builtins.functionArgs loader) (lib.attrNames available));
      in
      if unknown == [ ] then
        loader (builtins.intersectAttrs (builtins.functionArgs loader) available)
      else
        throw "module system: ${description} requested unavailable argument(s): ${lib.concatStringsSep ", " unknown}. Meta-modules receive only: ${lib.concatStringsSep ", " (lib.attrNames available)}.";

  # The fenced read surface: mounted plugins only, never the root's own
  # options. Reads work on every module that is *mounted* — enablement
  # gates effects, not visibility.
  global = lib.genAttrs pluginKeys (key: config.${key});

  evaluated = map (
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

  # Every module gets an implicit `enable` (plugin roots default on) that
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
        default = entry.isRoot;
        description = "Whether to activate `${entry.description}`.";
      };
    };

  splitBlocks =
    entry:
    let
      blocks = entry.applied.modules or { };
      unknown = lib.attrNames (removeAttrs blocks (lib.attrNames classMap));
    in
    if unknown == [ ] then
      {
        inline = lib.filterAttrs (block: _: classMap.${block} == class) blocks;
        foreign = lib.filterAttrs (block: _: classMap.${block} != class) blocks;
      }
    else
      throw "module system: ${entry.description} targets unknown module block `${lib.head unknown}`. Known blocks: ${lib.concatStringsSep ", " (lib.attrNames classMap)}.";

  # A fragment for the root's own class merges into the live fixpoint as
  # config. It gets the root's args (like any class fragment gets its
  # class's args) but cannot declare options or extend imports — config
  # cannot grow the eval. Anything needing that belongs at the assembly
  # site.
  inlineFragment =
    entry: block: fragment:
    let
      rootArgs = {
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
            unknown = lib.attrNames (removeAttrs (builtins.functionArgs fragment) (lib.attrNames rootArgs));
          in
          if unknown == [ ] then
            fragment (builtins.intersectAttrs (builtins.functionArgs fragment) rootArgs)
          else
            throw "module system: ${entry.description}'s `modules.${block}` requested unavailable argument(s): ${lib.concatStringsSep ", " unknown}. Root-class fragments receive only: ${lib.concatStringsSep ", " (lib.attrNames rootArgs)}.";

      body = if applied ? config then applied.config else applied;

      # Mounted plugins share this fixpoint, so a class block *could*
      # reach one. That reach is `plugins`' job — keeping it out of here
      # leaves `modules.<class>` meaning one thing: the host.
      reached = lib.filter (key: lib.elem key pluginKeys) (lib.attrNames body);
    in
    if applied ? options || applied ? imports then
      throw "module system: ${entry.description}'s `modules.${block}` runs in the live `${class}` fixpoint and cannot declare `options` or `imports`. Move those to the assembly site."
    else if reached != [ ] then
      throw "module system: ${entry.description}'s `modules.${block}` writes the plugin mounted at `${lib.head reached}`. Peer plugins go through `plugins`, not a class block."
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

  # `plugins.<handle>` writes a peer's namespace. It lands in the same
  # fixpoint `config` does, but a separate block means the reach is
  # declared rather than incidental — and an unmounted handle can say so
  # instead of surfacing as a missing option.
  peerWritesFor =
    entry:
    let
      writes = entry.applied.plugins or { };
      unknown = lib.filter (key: !(lib.elem key pluginKeys)) (lib.attrNames writes);
    in
    if unknown == [ ] then
      lib.mapAttrsToList (key: body: { ${key} = body; }) writes
    else
      throw "module system: ${entry.description} writes the plugin at `${lib.head unknown}`, which is not mounted. Register it alongside '${entry.binding}'.";

  # A module's `config` block is its plugin's namespace — the mount point
  # is implied, never spelled. Reaching out is explicit: `modules.<class>`
  # for the host, `plugins.<handle>` for a peer.
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
          _meta.fragments.${classMap.${block}} = [ (wrapFragment entry block fragment) ];
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
    options._meta = {
      fragments = lib.mkOption {
        type = lib.types.attrsOf (lib.types.listOf lib.types.deferredModule);
        description = ''
          Deferred class fragments per class tag, contributed by enabled
          modules. Installers route each class into its target eval (e.g.
          `home-manager.sharedModules`).
        '';
      };

      routed = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Class tags claimed by an installer.";
      };

      unrouted = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        readOnly = true;
        description = "Class tags holding fragments that no installer claimed.";
        default = lib.attrNames (
          lib.filterAttrs (
            tag: fragments: fragments != [ ] && !(lib.elem tag config._meta.routed)
          ) config._meta.fragments
        );
      };
    };

    config._meta.fragments = lib.genAttrs classTags (_: [ ]);
  };
in

{
  imports = map mountFor evaluated ++ [ bookkeeping ];
}
