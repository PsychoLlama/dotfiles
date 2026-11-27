# Neovim, assembled from what units say they want in it: plugins, language
# servers, and config. An unopinionated nixvim alternative. `runtime/` is the
# Lua `core` library that reads what is generated here.
{
  lib,
  rhizome,
  units,
  self,
  define,
  ...
}:

let
  t = rhizome.types;
  inherit (units.platform) home-manager user;

  optional = type: t.default (t.either type t.null) null;

  channel =
    type:
    define.fact {
      scope = user.scope;
      inherit type;
      merge = rhizome.merge.list;
      optional = true;
    };

  # With `pkgs`, for values that hold packages.
  resolve = pkgs: value: if lib.isFunction value then value pkgs else value;
in

{
  # The editor itself, given `pkgs`. Defaults to `pkgs.unstable.neovim`.
  neovim = define.fact {
    scope = user.scope;
    type = t.function;
    merge = rhizome.merge.one-or-none;
    optional = true;
  };

  # A plugin to install.
  plugin = channel {
    # Its attribute in `vimPlugins`, or in `pkgs.custom.vimPlugins`.
    name = t.str;

    # The package, given `pkgs`, when it isn't the one found by name.
    package = optional t.function;

    # A Lua file, or inline Lua. Runs after the plugin loads.
    config = optional t.any;

    # Passed to the config if it returns a function. Given `pkgs` if it is a
    # function itself.
    opts = t.default t.any { };

    # Load only once a trigger fires: `event`, `pattern`, `cmd`, `keys`, `ft`.
    defer = optional (t.attrs-of t.any);

    # It runs on Node.js.
    nodejs = t.default t.bool false;
  };

  # A language server. Fields mirror `vim.lsp.Config`.
  server = channel {
    name = t.str;

    # The executable and its arguments. Given `pkgs` if it is a function.
    cmd = t.either (t.list t.str) t.function;

    # Attach for these filetypes.
    filetypes = t.default (t.list t.str) [ ];

    # Search upward for these. The directory found is the workspace root.
    root_markers = t.default (t.list t.str) [ ];

    settings = optional t.attrs;
  };

  # Lines of `init.lua`. Given `pkgs` if it is a function.
  config = channel (t.either t.str t.function);

  # A program on the editor's `PATH`, given `pkgs`.
  tool = channel t.function;

  lib = {
    /**
      Build the editor from what was written to its channels.

      # Type

      ```
      build :: Pkgs -> {
        neovim :: (Pkgs -> Package)?,
        plugins :: [Plugin],
        servers :: [Server],
        config :: [String | (Pkgs -> String)],
        tools :: [Pkgs -> Package],
        trusted :: [String],
      } -> Package
      ```
    */
    build =
      pkgs:
      {
        neovim ? null,
        plugins ? [ ],
        servers ? [ ],
        config ? [ ],
        tools ? [ ],
        trusted ? [ ],
      }:

      let
        lua = lib.generators.toLua { };

        # `sep = null` keeps the flag and value as separate arguments, which
        # `makeWrapper` requires to read `--add-flags` and its value.
        optionFormat = name: {
          option = if builtins.stringLength name > 1 then "--${name}" else "-${name}";
          sep = null;
          explicitBool = false;
        };

        # Custom plugins shadow upstream ones.
        vimPlugins = name: pkgs.unstable.custom.vimPlugins.${name} or pkgs.unstable.vimPlugins.${name};

        installed = map (
          plugin:
          plugin
          // {
            package = if plugin.package == null then vimPlugins plugin.name else plugin.package pkgs;
          }
        ) (lib.sortOn (plugin: plugin.name) plugins);

        # Resolve a plugin config to a store path.
        configFile =
          plugin:
          if plugin.config == null then
            null
          else if lib.isPath plugin.config then
            # Isolates the file so unrelated repo changes don't invalidate the
            # editor derivation.
            builtins.path {
              name = "${plugin.package.pname}-config.lua";
              path = plugin.config;
            }
          else
            toString (pkgs.writeText "${plugin.package.pname}-config.lua" plugin.config);

        manifest = lib.sortOn (plugin: plugin.name) (
          map (plugin: {
            name = plugin.package.pname;
            source = plugin.package.outPath;
            opts = resolve pkgs plugin.opts;
            type = "pack";
            config = configFile plugin;
            inherit (plugin) defer;
          }) installed
        );

        clients = lib.listToAttrs (
          map (
            server:
            lib.nameValuePair server.name (
              lib.filterAttrs (_: value: value != null) {
                cmd = resolve pkgs server.cmd;
                inherit (server) filetypes root_markers settings;
              }
            )
          ) servers
        );

        init = pkgs.writeText "user-config.lua" (lib.concatStringsSep "\n" (map (resolve pkgs) config));

        # Isolate the runtime directory so changes elsewhere in the repo don't
        # invalidate the editor derivation. Tests (`*_spec.lua`) are excluded: they
        # pull in vusted/luassert, which aren't present during `buildVimPlugin`'s
        # require-check and would fail the build.
        runtime = lib.fileset.toSource {
          root = ./runtime;
          fileset = lib.fileset.difference ./runtime (
            lib.fileset.fileFilter (file: lib.hasSuffix "_spec.lua" file.name) ./runtime
          );
        };

        # This is the generated `&packpath` directory for all plugins.
        packdir = pkgs.unstable.vimUtils.packDir {
          managed-by-nix = {
            start = [
              # Core framework, automatically loaded. Provides package loading and
              # language server support.
              (pkgs.unstable.vimUtils.buildVimPlugin {
                pname = "neovim-core";
                version = "latest";
                src = runtime;
              })
            ];

            # Provide access to all plugins but don't load them immediately.
            opt = map (plugin: plugin.package) installed;
          };
        };
      in

      (if neovim == null then pkgs.unstable.neovim else neovim pkgs).override {
        withNodeJs = lib.any (plugin: plugin.nodejs) plugins;

        extraMakeWrapperArgs = lib.concatStringsSep " " [
          "--suffix PATH : ${lib.makeBinPath (map (tool: tool pkgs) tools)}"
          (lib.cli.toCommandLineShell optionFormat {
            add-flags = [
              ''--cmd "set packpath^=${packdir}"''
              ''--cmd "set rtp^=${packdir}"''
            ];
          })
        ];

        configure.customRC = ''
          lua << CORE_FRAMEWORK
          require('core.pkg._loader').set_manifest(${lua manifest})
          require('core.lsp').setup(${lua clients})
          require('core.env').setup(${lua { trusted_prefixes = trusted; }})
          CORE_FRAMEWORK

          source ${init}

          " Run per-plugin configs after the vimrc finishes.
          lua require('core.pkg._loader').eval_configs()
        '';
      };
  };

  # Builds the editor and installs it for the user.
  unit = define.unit {
    uses = {
      inherit (self) neovim;
      plugins = self.plugin;
      servers = self.server;
      config = self.config;
      tools = self.tool;

      # At or beneath one, a project-local vimrc is sourced without
      # prompting. `~` is expanded at runtime by the env framework.
      trusted = user.trusted-directory;
    };

    provides = collected: [
      (home-manager.module (
        { pkgs, ... }:

        let
          neovim = self.lib.build pkgs collected;
        in

        {
          home.packages = [ neovim ];

          home.sessionVariables = {
            EDITOR = "${neovim}/bin/nvim";
            MANPAGER = "${neovim}/bin/nvim -c 'Man!'";
          };

          programs.git.ignores = [ ".vimrc.lua" ];
        }
      ))
    ];
  };
}
