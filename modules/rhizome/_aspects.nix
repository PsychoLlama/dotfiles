{ lib, import-tree }:

let
  /**
    The module classes this flake's own aspects export to. Only a default: a
    consumer sweeping its own tree names whatever classes it invented.
  */
  defaultClasses = [
    "editor"
    "homeManager"
    "nixos"
  ];

  /**
    Render names as a backticked, comma-separated list for error messages.

    # Type

    ```
    quote :: [String] -> String
    ```
  */
  quote = names: lib.concatMapStringsSep ", " (name: "`${name}`") names;

  /**
    The id an aspect is published under: its path relative to the aspect root,
    without the `.nix` suffix, and without a trailing `default` naming a
    directory module.

    Throws if `path` is not under `root`, which is the failure the whole
    scheme rests on -- a mangled id would otherwise publish silently.

    # Type

    ```
    aspectId :: Path -> Path -> String
    ```

    # Examples

    ```nix
    aspectId ./aspects ./aspects/programs/nushell/default.nix
    => "programs/nushell"

    aspectId ./aspects ./aspects/programs/bat.nix
    => "programs/bat"
    ```
  */
  aspectId =
    root: path:
    let
      /**
        The path relative to `root`, split on `/`.
      */
      components = lib.path.subpath.components (lib.path.removePrefix root path);

      /**
        Everything above the file. Empty for an aspect sitting at the root.
      */
      directory = lib.init components;

      /**
        The file's own name, without the `.nix`.
      */
      basename = lib.removeSuffix ".nix" (lib.last components);
    in
    lib.concatStringsSep "/" (
      if basename == "default" && directory != [ ] then directory else directory ++ [ basename ]
    );

  /**
    Whether an `imports` entry names another aspect, as opposed to an inline
    module or a path outside the tree.

    # Type

    ```
    isAspectPath :: Path -> Any -> Bool
    ```
  */
  isAspectPath = root: entry: lib.isPath entry && lib.path.hasPrefix root entry;

  /**
    Return an aspect body unchanged, or throw if it sets anything but `exports`
    and `imports`, or exports to a class outside `classes`.

    # Inputs

    `classes`
    : The classes an aspect may export to.

    `name`
    : What to call the body when it fails. An aspect id, usually.

    `body`
    : The evaluated aspect.

    # Type

    ```
    checkExports :: [String] -> String -> AttrSet -> AttrSet
    ```
  */
  checkExports =
    classes: name: body:
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

  /**
    Turn one aspect file into a flake module publishing the aspect as
    `rhizomeModules.<id>.<class>`, one module per class.

    The aspect's own `imports` are routed by what each entry is:

    | entry                | treatment                                     |
    | :------------------- | :-------------------------------------------- |
    | a path under `root`  | recorded as a dependency id, **not** imported |
    | a value with no path | checked, and its exports fold into the parent |
    | any other path       | an ordinary flake module, passed through      |

    A dependency becomes an `imports` entry on the published module, pointing
    at the dependency's module for the same class -- so a profile already
    carries its whole transitive tree, and nothing has to walk it.

    # Inputs

    `classes`
    : The classes to publish under.

    `root`
    : The aspect root that ids are relative to.

    `path`
    : The aspect file.

    `args`
    : Flake module arguments. Read for `config.flake`, which is what lets
      dependencies resolve without a registry alongside.

    # Type

    ```
    import-aspect :: { classes :: [String], root :: Path } -> Path -> AttrSet -> Module
    ```
  */
  import-aspect =
    { classes, root }:
    path: args:

    let
      /**
        The flake's own outputs, closed over here so the class modules below
        can reach their dependencies. Referencing it from *their* `imports` is
        safe: they evaluate in another module system, where it is an ordinary
        value rather than the `config` being defined.
      */
      flake-outputs = args.config.flake;

      /**
        The aspect as written, before it is called or checked.
      */
      module = import path;

      /**
        The aspect called and validated. Only `exports` and `imports` survive.
      */
      body = checkExports classes id (if lib.isFunction module then module args else module);

      /**
        What this aspect publishes as.
      */
      id = aspectId root path;

      /**
        The aspect's `imports`, before routing.
      */
      entries = body.imports or [ ];

      /**
        Entries naming another aspect, as ids. Recorded rather than imported:
        the sweep already published them, and importing the file again would
        be a second module key for the same path.
      */
      dependencies = map (aspectId root) (lib.filter (isAspectPath root) entries);

      /**
        Entries with no path of their own, so no id to publish under. Their
        exports fold into this aspect's instead.
      */
      inlined = lib.filter (entry: !(lib.isPath entry)) entries;

      /**
        Everything else -- platform extensions, `rhizome/` options. Ordinary
        flake modules, passed through untouched.
      */
      foreign = lib.filter (entry: lib.isPath entry && !(isAspectPath root entry)) entries;

      /**
        `exports` harvested from the inlined entries, each checked like an aspect.
      */
      inlinedExports = map (
        entry:
        (checkExports classes "${id}'s inline import" (if lib.isFunction entry then entry args else entry))
        .exports or { }
      ) inlined;

      /**
        This aspect's exports merged with the inlined ones, per class. Merging
        as `imports` rather than by attribute keeps each contribution a module
        in its own right, so their options and defaults resolve normally.
      */
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
      # The explicit `key` is what makes dependency resolution safe: two
      # aspects depending on the same third one contribute the same key, so
      # the module system loads it once. Without it every route would be a
      # fresh module, and the file's options declared twice.
      flake.rhizomeModules.${id} = lib.genAttrs classes (class: {
        _class = class;
        _file = path;
        key = "aspect:${class}:${id}";

        imports = [
          (exports.${class} or { })
        ]
        ++ map (dependency: flake-outputs.rhizomeModules.${dependency}.${class}) dependencies;
      });
    };

  /**
    Sweep a directory of aspects into a single flake module, publishing every
    file in it as `rhizomeModules.<id>.<class>`.

    Paths containing a `/_` segment are skipped, which is what keeps helpers
    and data files out.

    # Inputs

    `root`
    : The directory to sweep. Taken once rather than twice: the sweep and the
      id derivation must agree on it, and two roots that disagree would
      publish every aspect under a mangled id with nothing to catch it.

    Structured function argument:

    `classes`
    : The classes to publish under, and the closed set an aspect's `exports`
      are checked against. Defaults to `defaultClasses`.

    # Type

    ```
    import-aspects :: Path -> { classes :: [String] } -> Module
    ```

    # Examples

    ```nix
    imports = [ (import-aspects ./aspects { }) ];
    ```
  */
  import-aspects =
    root:
    {
      classes ? defaultClasses,
    }:
    let
      importer = import-aspect { inherit classes root; };
    in
    (import-tree.map importer) root;
in

{
  inherit import-aspects;
}
