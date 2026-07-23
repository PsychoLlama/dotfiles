{ lib }:

let
  builtinClasses = import ./classes.nix;
in

/*
  The root guest: a module that mounts every plugin module's options into
  the host root's own fixpoint, keyed by handle.

  There is no separate meta eval. The guest evaluates exactly once, on
  the top-level root — nixos when there is one, home-manager standalone,
  or any custom `evalModules` (the editor). Fragments for the root's own
  class merge inline; fragments for every other class accumulate in
  `_meta.fragments.<class>` for an installer to route onward.

  Meta modules receive exactly five arguments — `self`, `cfg`, `global`,
  `lib`, `pkgs` — and nothing from the root. `global` is fenced to the
  mounted handles, so a module can never observe (and grow dependent on)
  the host platform it happens to be evaluated in.

  Type: { class : String, plugins : AttrSet Plugin } -> Module
*/
{
  class,
  plugins,
}:

let
  pluginList = lib.mapAttrsToList (binding: plugin: { inherit binding plugin; }) plugins;

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
      throw "module system: plugin '${entry.binding}' remaps platform block `${lib.head (lib.attrNames conflicts)}` to a different class tag.";

  # platform block key -> `_class` tag, across built-ins and all plugins.
  classMap = lib.foldl' mergeClasses builtinClasses pluginList;
  classTags = lib.unique (lib.attrValues classMap);

  # One entry per mounted option namespace: every module of every plugin,
  # plus each plugin's root node. Registering the same plugin under two
  # bindings dedupes by store path.
  entries = lib.attrValues (
    lib.listToAttrs (
      map (entry: lib.nameValuePair entry.key entry) (
        lib.concatMap (
          { binding, plugin }:
          map (mod: {
            inherit binding plugin;
            key = toString mod.file;
            loader = import mod.file;
            description = "${binding}.${lib.concatStringsSep "." mod.subpath}";
            isRoot = false;
          }) plugin.__plugin.modules
          ++ lib.optional (plugin.__plugin.root != null) {
            inherit binding plugin;
            key = plugin.__plugin.key;
            loader = plugin.__plugin.root;
            description = binding;
            isRoot = true;
          }
        ) pluginList
      )
    )
  );

  handleKeys = map (entry: entry.key) entries;
in

{
  config,
  options,
  pkgs,
  ...
}:

let
  # The fenced read surface: only mounted handles, never the root's own
  # options. Reads work on every module that is *mounted* — enablement
  # gates effects, not visibility.
  global = lib.genAttrs handleKeys (key: config.${key});

  applyEntry =
    entry:
    if !lib.isFunction entry.loader then
      entry.loader
    else
      let
        args = {
          self = entry.plugin.__plugin.namespace;
          cfg = config.${entry.key};
          inherit global lib pkgs;
        };
        unknown = lib.attrNames (removeAttrs (builtins.functionArgs entry.loader) (lib.attrNames args));
      in
      if unknown == [ ] then
        entry.loader (builtins.intersectAttrs (builtins.functionArgs entry.loader) args)
      else
        throw "module system: ${entry.description} requested unavailable argument(s): ${lib.concatStringsSep ", " unknown}. Meta-modules receive only: ${lib.concatStringsSep ", " (lib.attrNames args)}.";

  evaluated = map (entry: entry // { applied = applyEntry entry; }) entries;

  # Every module gets an implicit `enable` (plugin roots default on) that
  # gates its writes and platform blocks — loading is never the cut.
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

  keyIndex = lib.genAttrs handleKeys (_: true);

  # Find the top-level keys a `config` block writes, looking through the
  # merge machinery a module may legitimately wrap around it.
  writtenKeys =
    value:
    let
      kind = value._type or null;
    in
    if kind == "if" || kind == "override" || kind == "order" then
      writtenKeys value.content
    else if kind == "merge" then
      lib.concatMap writtenKeys value.contents
    else
      lib.attrNames value;

  # A meta-module's `config` block may only address other plugin modules.
  # Host configuration must go through a `platforms.<class>` block, where
  # the target platform is explicit.
  checkWrites =
    entry: writes:
    let
      violations = lib.filter (key: !(keyIndex ? ${key})) (writtenKeys writes);
    in
    if violations == [ ] then
      writes
    else
      throw "module system: ${entry.description} writes to `${lib.head violations}`, which is not a plugin module. Use a `platforms.<class>` block for host configuration.";

  splitPlatforms =
    entry:
    let
      blocks = entry.applied.platforms or { };
      unknown = lib.attrNames (removeAttrs blocks (lib.attrNames classMap));
    in
    if unknown == [ ] then
      {
        inline = lib.filterAttrs (block: _: classMap.${block} == class) blocks;
        foreign = lib.filterAttrs (block: _: classMap.${block} != class) blocks;
      }
    else
      throw "module system: ${entry.description} targets unknown platform block `${lib.head unknown}`. Known blocks: ${lib.concatStringsSep ", " (lib.attrNames classMap)}.";

  # A fragment for the root's own class merges into the live fixpoint as
  # config. It gets the root's args (like any platform fragment gets its
  # platform's args) but cannot declare options or extend imports —
  # config cannot grow the eval. Anything needing that belongs at the
  # assembly site.
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
            throw "module system: ${entry.description}'s `platforms.${block}` requested unavailable argument(s): ${lib.concatStringsSep ", " unknown}. Root-class fragments receive only: ${lib.concatStringsSep ", " (lib.attrNames rootArgs)}.";
    in
    if applied ? options || applied ? imports then
      throw "module system: ${entry.description}'s `platforms.${block}` runs in the live `${class}` fixpoint and cannot declare `options` or `imports`. Move those to the assembly site."
    else if applied ? config then
      applied.config
    else
      applied;

  # Fragments for every other class are deferred modules: they cross into
  # a fresh eval with full module powers, tagged so importing one into
  # the wrong platform fails loudly.
  wrapFragment = entry: block: fragment: {
    _file = "${entry.key}#platforms.${block}";
    _class = classMap.${block};
    imports = [ fragment ];
  };

  contributionFor =
    entry:
    let
      split = splitPlatforms entry;
    in
    lib.mkIf config.${entry.key}.enable (
      lib.mkMerge (
        [ (checkWrites entry (entry.applied.config or { })) ]
        ++ lib.mapAttrsToList (block: fragment: inlineFragment entry block fragment) split.inline
        ++ lib.mapAttrsToList (block: fragment: {
          _meta.fragments.${classMap.${block}} = [ (wrapFragment entry block fragment) ];
        }) split.foreign
      )
    );
in

{
  options =
    lib.listToAttrs (map (entry: lib.nameValuePair entry.key (optionsFor entry)) evaluated)
    // {
      _meta = {
        fragments = lib.mkOption {
          type = lib.types.attrsOf (lib.types.listOf lib.types.deferredModule);
          description = ''
            Deferred platform fragments per class tag, contributed by
            enabled modules. Installers route each class into its target
            eval (e.g. `home-manager.sharedModules`).
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
    };

  config = lib.mkMerge (
    [ { _meta.fragments = lib.genAttrs classTags (_: [ ]); } ] ++ map contributionFor evaluated
  );
}
