{ lib }:

let
  classes = [
    "editor"
    "homeManager"
    "nixos"
  ];

  quote = names: lib.concatMapStringsSep ", " (name: "`${name}`") names;

  # `modules/aspects/programs/nushell/default.nix` -> `programs/nushell`
  aspectId =
    root: path:
    lib.pipe (toString path) [
      (lib.removePrefix "${toString root}/")
      (lib.removeSuffix ".nix")
      (lib.removeSuffix "/default")
    ];

  isAspectPath = root: entry: lib.isPath entry && lib.hasPrefix "${toString root}/" (toString entry);

  checkExports =
    name: body:
    let
      strayKeys = lib.subtractLists [ "exports" "imports" ] (lib.attrNames body);
      strayClasses = lib.subtractLists classes (lib.attrNames (body.exports or { }));
    in
    lib.throwIf (strayKeys != [ ])
      "Aspect `${name}` sets ${quote strayKeys}. An aspect may only set `exports` and `imports`."
      (
        lib.throwIf (strayClasses != [ ]) "Aspect `${name}` exports unknown ${
          if lib.length strayClasses == 1 then "class" else "classes"
        } ${quote strayClasses}. Known classes are ${quote classes}." body
      );

  import-aspect =
    root: path: args:

    let
      module = import path;
      body = checkExports id (if lib.isFunction module then module args else module);

      id = aspectId root path;
      entries = body.imports or [ ];

      # An entry naming another aspect becomes a dependency, recorded as an id
      # rather than a path -- the sweep already published it. An entry with no
      # path of its own has no id, so its exports fold into ours. Anything else
      # is an ordinary flake module (platform extensions, rhizome options) and
      # passes through untouched.
      dependencies = map (aspectId root) (lib.filter (isAspectPath root) entries);
      inlined = lib.filter (entry: !(lib.isPath entry)) entries;
      foreign = lib.filter (entry: lib.isPath entry && !(isAspectPath root entry)) entries;

      inlinedExports = map (
        entry:
        (checkExports "${id}'s inline import" (if lib.isFunction entry then entry args else entry)).exports
          or { }
      ) inlined;

      exports = lib.zipAttrsWith (_class: exported: { imports = exported; }) (
        [ (body.exports or { }) ] ++ inlinedExports
      );
    in

    {
      _file = path;
      key = path;

      imports = foreign;

      # Published to every class, including ones the aspect never exports. An
      # empty module costs nothing, and it means `load-modules` can walk a
      # pure aggregator -- which exports nothing at all -- like any other id.
      flake.modules = lib.genAttrs classes (class: {
        ${id} = exports.${class} or { };
      });

      # The dependency graph is tracked here rather than on the module itself:
      # `flake.modules` runs its definitions through `deferredModule`, which
      # rewraps them, so nothing smuggled alongside a module survives.
      rhizome.aspects.${id} = { inherit dependencies; };
    };

  # `genericClosure` dedups by key, so an import cycle terminates here rather
  # than hanging. Whether a cycle is *legal* is a separate question, deferred
  # until one actually shows up.
  mkLoadModules =
    {
      aspects,
      modules,
      root,
    }:
    class: target:

    let
      id = if lib.isString target then target else aspectId root target;

      closure = builtins.genericClosure {
        startSet = [ { key = id; } ];
        operator = { key }: map (dependency: { key = dependency; }) aspects.${key}.dependencies;
      };
    in

    lib.throwIf (!(aspects ? ${id})) "No aspect named `${id}`." {
      imports = map ({ key }: modules.${class}.${key}) closure;
    };
in

{
  inherit
    classes
    aspectId
    import-aspect
    mkLoadModules
    ;
}
