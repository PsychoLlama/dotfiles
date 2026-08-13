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
      # The flake's own `config`, closed over here so the class modules below
      # can reach their dependencies. Referencing it from *their* `imports` is
      # safe: they evaluate in another module system, where it is a plain value.
      flake-modules = args.config.flake.modules;

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

      # Published to every class, including ones the aspect never exports: an
      # empty module costs nothing, and it keeps a pure aggregator -- which
      # exports nothing at all -- reachable like any other id.
      #
      # Dependencies are resolved here rather than tracked in a registry. The
      # explicit `key` is what makes that safe: two aspects depending on the
      # same third one contribute the same key, so the module system loads it
      # once. Without it every route to a dependency would be a fresh module.
      flake.modules = lib.genAttrs classes (class: {
        ${id} = {
          _file = path;
          key = "aspect:${class}:${id}";

          imports = [
            (exports.${class} or { })
          ]
          ++ map (dependency: flake-modules.${class}.${dependency}) dependencies;
        };
      });
    };

in

{
  inherit
    classes
    aspectId
    import-aspect
    ;
}
