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
  plugins."${inputs.hosts}".machines.x.enable = true; # another plugin
  ```

  Rhizome modules receive exactly six arguments — `self`, `cfg`,
  `inputs`, `global`, `lib`, `pkgs` — and nothing from the host.
  `global` is fenced to the mounted plugins, so a module can never
  observe (and grow dependent on) the host platform it happens to be
  evaluated in.

  This is the mount itself, taking its class as an argument: use it for
  a class rhizome ships no root for, and carry or drop the fragments
  yourself. `configure` is where that goes — an ordinary module, merged
  in alongside the plugins, so a root comes back as one value instead of
  a list the caller has to assemble:

  ```nix
  mounts.custom {
    class = "editor";
    plugins = { inherit dotfiles; };
    configure.rhizome.dropped = [ "nixos" "darwin" "homeManager" ];
  }
  ```

  The siblings in this directory are that same call with a stack's
  routers and drop policy already passed in.

  This file is the assembly. The parts it draws on, one directory up:

  - `load-plugins.nix` — what is mounted, resolved before any eval.
    Defines the `Entry` record the rest of the mount passes around.
  - `apply-module.nix` — applying a module to its closed set of
    arguments.
  - `module-writes.nix` — where one module's writes land.
  - `fragment-options.nix` — the mount's own `rhizome.*` options.

  # Type

  ```
  mounts.custom :: {
    class : String,               # `_class` of the eval this mounts into
    plugins : AttrSet Plugin,     # instantiated plugins, by binding name
    configure? : Module,          # merged in alongside them
  } -> Module
  ```
*/

{
  class,
  plugins,
  configure ? { },
}:

let
  applyModule = import ../apply-module.nix { inherit lib; };

  inherit (import ../load-plugins.nix { inherit lib; } plugins)
    entries
    classMap
    classTags
    pluginKeys
    ;
in

{
  config,
  options,
  pkgs,
  ...
}:

let
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

  writesFor =
    import ../module-writes.nix { inherit lib; }
      {
        inherit class classMap pluginKeys;
      }
      {
        inherit config options pkgs;
      };

  /**
    The whole surface of a rhizome module. Every write is read by name,
    so a key nobody reads is not an extension point — it is a typo that
    merges into nothing while the module appears to have done its job.

    # Type

    ```
    moduleKeys :: [ String ]
    ```
  */
  moduleKeys = [
    "options" # declared onto the mount point by `optionsFor`
    "config" # this plugin's own namespace
    "modules" # a host class: inline here, or a fragment to route
    "plugins" # another mounted plugin's namespace
  ];

  /**
    Reject a module whose top level names anything unreadable. Checked
    where the module is applied rather than where its writes are read,
    so an unenabled module still has to be spelled correctly.

    # Type

    ```
    checkKeys :: String -> AttrSet -> AttrSet
    ```
  */
  checkKeys =
    description: applied:
    let
      unknown = lib.attrNames (lib.removeAttrs applied moduleKeys);
    in
    if unknown == [ ] then
      applied
    else
      throw "rhizome: ${description} has unrecognised top-level key(s): ${lib.concatStringsSep ", " unknown}. Modules write through: ${lib.concatStringsSep ", " moduleKeys}.";

  /**
    Every entry with its module applied, ready to contribute options and
    config.

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
      applied = checkKeys entry.description (
        applyModule {
          inherit (entry) description;
          subject = "Modules";
          available = {
            self = config.${lib.head entry.path};
            cfg = lib.getAttrFromPath entry.path config;
            inputs = entry.plugin.__plugin.inputs;
            inherit global lib pkgs;
          };
        } entry.loader
      );
    }
  ) entries;

  /**
    A module's declared options, plus the implicit `enable` that gates
    its writes — loading is never the cut. A module may declare `enable`
    itself to change its type or default; the plugin itself defaults on,
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
        default = entry.isPlugin;
        description = "Whether to activate `${entry.description}`.";
      };
    };

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
    config = writesFor entry;
  };
in

{
  imports = lib.map mountFor evaluated ++ [
    (import ../fragment-options.nix { inherit lib; } classTags)
    configure
  ];
}
