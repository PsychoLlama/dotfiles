{ lib, moduleLocation, ... }:

let
  /**
    Declare a flake output holding modules of one class, matching the shape
    flake-parts gives `nixosModules`.

    flake-parts only ships that one. An undeclared flake output is typed
    `unique raw`, so without a declaration here the *second* definition of
    `homeModules.<id>` fails to merge rather than adding an attribute.

    # Inputs

    `class`
    : The module class, stamped onto every module so a mismatch is caught
      where it is loaded rather than wherever its options finally collide.

    `output`
    : The flake output attribute, used to label modules in error messages.

    # Type

    ```
    moduleOutput :: { class :: String, output :: String } -> Option
    ```
  */
  moduleOutput =
    { class, output }:
    lib.mkOption {
      description = "`${class}` modules published by the flake.";
      type = lib.types.lazyAttrsOf lib.types.deferredModule;
      default = { };

      apply = lib.mapAttrs (
        id: module: {
          _class = class;
          _file = "${toString moduleLocation}#${output}.${id}";
          imports = [ module ];
        }
      );
    };
in

{
  options.flake = {
    editorModules = moduleOutput {
      class = "editor";
      output = "editorModules";
    };

    homeModules = moduleOutput {
      class = "homeManager";
      output = "homeModules";
    };

    # Classes are left open: a downstream sweep names whatever classes it
    # invented, and the loader stamps `_class` on each module it publishes.
    rhizomeModules = lib.mkOption {
      description = ''
        Aspects published by the flake, keyed by id. Each holds one module per
        class, already carrying its dependencies.
      '';

      type = lib.types.lazyAttrsOf (lib.types.lazyAttrsOf lib.types.deferredModule);
      default = { };
    };
  };
}
