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

  Meta modules receive exactly five arguments — `self`, `cfg`, `global`,
  `lib`, `pkgs` — and nothing from the root. `global` is fenced to the
  mounted plugins, so a module can never observe (and grow dependent on)
  the host platform it happens to be evaluated in.

  Type: { class : String, plugins : AttrSet Plugin } -> Module
*/
{
  class,
  plugins,
}:

let
  # Registering the same plugin under two bindings mounts it once. The
  # binding name is only ever used for error messages.
  pluginList = lib.attrValues (
    lib.listToAttrs (
      lib.mapAttrsToList (
        binding: plugin: lib.nameValuePair plugin.__plugin.key { inherit binding plugin; }
      ) plugins
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
      # first hop is strict and loud on typos.
      applied = applyModule entry.description {
        self = config.${lib.head entry.path};
        cfg = lib.getAttrFromPath entry.path config;
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
    in
    if applied ? options || applied ? imports then
      throw "module system: ${entry.description}'s `modules.${block}` runs in the live `${class}` fixpoint and cannot declare `options` or `imports`. Move those to the assembly site."
    else if applied ? config then
      applied.config
    else
      applied;

  # Fragments for every other class are deferred modules: they cross into
  # a fresh eval with full module powers, tagged so importing one into
  # the wrong platform fails loudly.
  wrapFragment = entry: block: fragment: {
    _file = "${entry.file}#modules.${block}";
    _class = classMap.${block};
    imports = [ fragment ];
  };

  # A module's `config` block is its plugin's namespace — the mount point
  # is implied, never spelled. Reaching anything else (the host, a peer
  # plugin) goes through `modules.root`, where the target is explicit.
  contributionFor =
    entry:
    let
      split = splitBlocks entry;
    in
    lib.mkIf (lib.getAttrFromPath (entry.path ++ [ "enable" ]) config) (
      lib.mkMerge (
        [ { ${lib.head entry.path} = entry.applied.config or { }; } ]
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
