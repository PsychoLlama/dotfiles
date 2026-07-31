{ lib }:

/**
  The mount's own options: where foreign fragments collect, who claimed
  them, and what nobody did. Declared once for the whole mount rather
  than per module, and separately from the plugins so a root can be
  wired up (or audited) without reference to what is mounted.

  # Type

  ```
  fragmentOptions :: [String] -> Module
  ```
*/

classTags:

{ config, options, ... }:

{
  options.rhizome = {
    fragments = lib.mkOption {
      type = lib.types.attrsOf (lib.types.listOf lib.types.deferredModule);
      description = ''
        Deferred class fragments per class tag, contributed by enabled
        modules. Routers carry each class into its target eval (e.g.
        `home-manager.sharedModules`).
      '';
    };

    routed = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Class tags claimed by a router.";
    };

    dropped = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = ''
        Class tags whose fragments are deliberately discarded. A root
        declares the classes that can never apply to its stack — a nixos
        host has no use for darwin fragments — so that whatever is left
        over reads as an oversight rather than a choice.
      '';
    };

    unrouted = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      readOnly = true;
      description = ''
        Class tags holding fragments that no router claimed and no root
        discarded: configuration that was written and then went nowhere.
      '';
      default =
        let
          claimed = config.rhizome.routed ++ config.rhizome.dropped;

          # A typo here would silently fail to claim, which is the exact
          # silence the assertion downstream exists to remove.
          unknown = lib.subtractLists classTags claimed;
        in
        if unknown != [ ] then
          throw "rhizome: `rhizome.routed`/`rhizome.dropped` names unknown class tag(s): ${lib.concatStringsSep ", " unknown}. Known tags: ${lib.concatStringsSep ", " classTags}."
        else
          lib.pipe config.rhizome.fragments [
            (lib.filterAttrs (tag: fragments: fragments != [ ] && !(lib.elem tag claimed)))
            lib.attrNames
          ];
    };
  };

  config = lib.mkMerge [
    { rhizome.fragments = lib.genAttrs classTags (_: [ ]); }

    # Configuration that was written and then went nowhere is a failure,
    # not a warning — but only a host with an assertions mechanism can
    # say so. A custom class without one has to read `rhizome.unrouted`
    # itself; there is nothing here that could force the check, since
    # nothing in a bare `evalModules` is obliged to read it.
    (lib.optionalAttrs (options ? assertions) {
      assertions = lib.map (tag: {
        assertion = false;
        message = "rhizome: class '${tag}' produced fragments, but no router claimed them. Carry it with `rhizome.routed = [ \"${tag}\" ];` after wiring the fragments into a `${tag}` eval, or discard it deliberately with `rhizome.dropped = [ \"${tag}\" ];`.";
      }) config.rhizome.unrouted;
    })
  ];
}
