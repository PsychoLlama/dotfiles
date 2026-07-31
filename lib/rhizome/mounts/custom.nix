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

  This is the mount itself, taking its class as an argument: use it for
  a class rhizome ships no root for, and carry or drop the fragments
  yourself. The siblings in this directory are that same machinery with
  a stack's routers and drop policy already attached.

  This file is the assembly. The parts it draws on, one directory up:

  - `load-plugins.nix` — what is mounted, resolved before any eval.
    Defines the `Entry` record the rest of the mount passes around.
  - `apply-module.nix` — applying a module to its closed set of
    arguments.
  - `module-writes.nix` — where one module's writes land.
  - `fragment-options.nix` — the mount's own `rhizome.*` options.

  # Type

  ```
  mounts.custom :: { class : String, plugins : AttrSet Plugin } -> Module
  ```
*/

{
  class,
  plugins,
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
      applied = applyModule {
        inherit (entry) description;
        subject = "Modules";
        available = {
          self = config.${lib.head entry.path};
          cfg = lib.getAttrFromPath entry.path config;
          inputs = entry.plugin.__plugin.inputs;
          inherit global lib pkgs;
        };
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
  ];
}
