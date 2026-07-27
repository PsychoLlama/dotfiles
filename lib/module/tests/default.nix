{ lib }:

let
  plugin = import ../plugin.nix { inherit lib; };
  mkRoot = import ../mk-root.nix { inherit lib; };

  main = plugin {
    src = ./fixtures/main/modules;

    classes = {
      test = "test";
      widget = "widget";
    };

    root =
      {
        self,
        cfg,
        lib,
        ...
      }:
      {
        options.themeName = lib.mkOption {
          type = lib.types.str;
          default = "plain";
        };

        config."${self}".theme.name = cfg.themeName;
      };
  };

  # A second plugin, to prove `global` is keyed by plugin and that two
  # namespaces coexist without collision.
  #
  # Cross-plugin wiring lives in the root node because that is where other
  # plugins are in scope — a discovered module file has no way to name one.
  side = plugin {
    src = ./fixtures/side/modules;

    root =
      {
        global,
        lib,
        ...
      }:
      {
        options.observed = lib.mkOption {
          type = lib.types.str;
          readOnly = true;
          default = global."${main}".theme.name;
        };

        config."${main}".services.beta.message = "from the side";
      };
  };

  # A minimal host root standing in for nixos/home-manager: declares one
  # native option so tests can watch root-class fragments land (and prove
  # `global` never exposes it).
  evalRoot =
    plugins: modules:
    lib.evalModules {
      class = "test";
      modules = [
        (mkRoot {
          class = "test";
          inherit plugins;
        })
        {
          config._module.args.pkgs = { };

          options.hostSetting = lib.mkOption {
            type = lib.types.str;
            default = "";
          };
        }
      ]
      ++ modules;
    };

  base = evalRoot { inherit main; } [ ];

  enabled = evalRoot { inherit main; } [
    { config."${main}".programs.alpha.enable = true; }
  ];

  paired = evalRoot { inherit main side; } [
    { config."${side}".probe.enable = true; }
  ];

  # A downstream eval consuming collected fragments, the way home-manager
  # consumes `sharedModules`.
  buildWidget =
    fragments:
    lib.evalModules {
      class = "widget";
      modules = [
        {
          config._module.args.prefix = "W";

          options.label = lib.mkOption {
            type = lib.types.str;
            default = "";
          };
        }
      ]
      ++ fragments;
    };

  fails = expr: !(builtins.tryEval (builtins.deepSeq expr expr)).success;
in

lib.runTests {
  # ── Discovery and the namespace tree ──────────────────────────────────

  testNamespaceShape = {
    expr = lib.sort lib.lessThan (
      lib.attrNames (
        removeAttrs main [
          "__plugin"
          "__toString"
        ]
      )
    );
    expected = [
      "introspect"
      "programs"
      "services"
      "theme"
    ];
  };

  testNamespaceNests = {
    expr = main.programs ? alpha;
    expected = true;
  };

  # The plugin is the only handle: modules are reached by navigating from
  # it, not by stringifying them.
  testPluginRootIsAddressable = {
    expr = lib.hasSuffix "/main/modules" "${main}";
    expected = true;
  };

  testModulesAreNotAddressable = {
    expr = main.programs.alpha ? __toString;
    expected = false;
  };

  testDuplicateMountPointsRejected = {
    expr = fails (plugin { src = ./fixtures/dup/modules; }).__plugin.modules;
    expected = true;
  };

  # ── Enablement gates effects, not visibility ──────────────────────────

  testModulesDisabledByDefault = {
    expr = base.config."${main}".programs.alpha.enable;
    expected = false;
  };

  testDisabledWritesAreInert = {
    expr = base.config."${main}".services.beta.enable;
    expected = false;
  };

  testEnableCascadesToPeers = {
    expr = enabled.config."${main}".services.beta.message;
    expected = "hello from alpha";
  };

  testReadsNeedMountingNotEnabling = {
    # alpha reads theme through `self` while theme stays disabled.
    expr = enabled.config."${main}".programs.alpha.summary;
    expected = "hello on #000000";
  };

  # ── The plugin root node ──────────────────────────────────────────────

  testRootNodeForwardsConfig = {
    expr =
      (evalRoot { inherit main; } [ { config."${main}".themeName = "nord"; } ])
      .config."${main}".theme.name;
    expected = "nord";
  };

  testRootNodeValidatesOptions = {
    expr =
      fails
        (evalRoot { inherit main; } [ { config."${main}".themeName = 42; } ]).config."${main}".theme.name;
    expected = true;
  };

  # Root options share the plugin's namespace with its modules, so `self`
  # reaches both.
  testRootOptionsJoinSelf = {
    expr = base.config."${main}".programs.alpha.rootView;
    expected = "plain";
  };

  # ── Platform fragments ────────────────────────────────────────────────

  testRootClassFragmentInlines = {
    expr = enabled.config.hostSetting;
    expected = "alpha was here";
  };

  testRootClassFragmentGatedByEnable = {
    expr = base.config.hostSetting;
    expected = "";
  };

  testForeignFragmentsCollected = {
    expr = lib.length enabled.config._meta.fragments.widget;
    expected = 1;
  };

  testForeignFragmentsGatedByEnable = {
    expr = base.config._meta.fragments.widget;
    expected = [ ];
  };

  testFragmentEvaluatesInTargetClass = {
    # The deferred fragment takes the widget eval's args (`prefix`) while
    # closing over meta scope (`cfg.greeting`).
    expr = (buildWidget enabled.config._meta.fragments.widget).config.label;
    expected = "W: hello";
  };

  testFragmentsRejectWrongClassEval = {
    # The `_class` tag makes importing a fragment into the wrong platform
    # fail loudly.
    expr =
      fails
        (lib.evalModules {
          class = "nixos";
          modules = enabled.config._meta.fragments.widget;
        }).config;
    expected = true;
  };

  # ── Installer bookkeeping ─────────────────────────────────────────────

  testUnroutedClassesReported = {
    expr = enabled.config._meta.unrouted;
    expected = [ "widget" ];
  };

  testRoutedClassesClearUnrouted = {
    expr =
      (evalRoot { inherit main; } [
        {
          config."${main}".programs.alpha.enable = true;
          config._meta.routed = [ "widget" ];
        }
      ]).config._meta.unrouted;
    expected = [ ];
  };

  # ── Fencing ───────────────────────────────────────────────────────────

  testGlobalHidesRootOptions = {
    expr = base.config."${main}".introspect.fenced;
    expected = true;
  };

  testSelfAgreesWithGlobal = {
    expr = base.config."${main}".introspect.reflexive;
    expected = true;
  };

  testHostWritesRejected = {
    expr =
      fails
        (evalRoot { bad = plugin { src = ./fixtures/bad-write/modules; }; } [ ]).config._meta.fragments;
    expected = true;
  };

  testUnknownArgsRejected = {
    expr =
      fails
        (evalRoot { bad = plugin { src = ./fixtures/bad-args/modules; }; } [ ]).config._meta.fragments;
    expected = true;
  };

  testUnknownPlatformBlockRejected = {
    expr =
      fails
        (evalRoot { bad = plugin { src = ./fixtures/bad-class/modules; }; } [ ]).config._meta.fragments;
    expected = true;
  };

  # ── Cross-plugin ──────────────────────────────────────────────────────

  testPluginsMountSideBySide = {
    expr = paired.config."${side}".probe.enable && !paired.config."${main}".programs.alpha.enable;
    expected = true;
  };

  # A module addresses another plugin exactly the way it addresses its
  # own: by that plugin's handle, through `global`.
  testCrossPluginRead = {
    expr = paired.config."${side}".observed;
    expected = "plain";
  };

  testCrossPluginWrite = {
    expr = paired.config."${main}".services.beta.message;
    expected = "from the side";
  };
}
