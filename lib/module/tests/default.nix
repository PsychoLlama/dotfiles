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

        config."${self.theme}".name = cfg.themeName;
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
    { config."${main.programs.alpha}".enable = true; }
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
  # ── Discovery and handles ─────────────────────────────────────────────

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

  testHandleIsModulePath = {
    expr = lib.hasSuffix "/programs/alpha/mod.nix" "${main.programs.alpha}";
    expected = true;
  };

  testPluginRootIsAddressable = {
    expr = lib.hasSuffix "/main/modules" "${main}";
    expected = true;
  };

  testDuplicateMountPointsRejected = {
    expr = fails (plugin { src = ./fixtures/dup/modules; }).__plugin.modules;
    expected = true;
  };

  # ── Enablement gates effects, not visibility ──────────────────────────

  testModulesDisabledByDefault = {
    expr = base.config."${main.programs.alpha}".enable;
    expected = false;
  };

  testDisabledWritesAreInert = {
    expr = base.config."${main.services.beta}".enable;
    expected = false;
  };

  testEnableCascadesToPeers = {
    expr = enabled.config."${main.services.beta}".message;
    expected = "hello from alpha";
  };

  testReadsNeedMountingNotEnabling = {
    # alpha reads theme through `global` while theme stays disabled.
    expr = enabled.config."${main.programs.alpha}".summary;
    expected = "hello on #000000";
  };

  # ── The plugin root node ──────────────────────────────────────────────

  testRootNodeForwardsConfig = {
    expr =
      (evalRoot { inherit main; } [ { config."${main}".themeName = "nord"; } ])
      .config."${main.theme}".name;
    expected = "nord";
  };

  testRootNodeValidatesOptions = {
    expr =
      fails
        (evalRoot { inherit main; } [ { config."${main}".themeName = 42; } ]).config."${main.theme}".name;
    expected = true;
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
          config."${main.programs.alpha}".enable = true;
          config._meta.routed = [ "widget" ];
        }
      ]).config._meta.unrouted;
    expected = [ ];
  };

  # ── Fencing ───────────────────────────────────────────────────────────

  testGlobalHidesRootOptions = {
    expr = base.config."${main.introspect}".fenced;
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
}
