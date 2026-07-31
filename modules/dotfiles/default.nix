{ rhizome }:

# The opinions: one module per program, carrying payloads for every
# platform it touches. Knows nothing about hosts.
rhizome.plugin {
  src = ./.;
  classes.editor = "editor";

  # Declaring a class means routing it. On `configure`, so it holds
  # whether or not anything opted in. Fragments only exist in the
  # fixpoint the mount evaluated in, so this is `nixos`: on a host they
  # land an eval above the editor, and `sharedModules` is the way down.
  configure.modules.nixos =
    {
      config,
      options,
      lib,
      ...
    }:

    # Nowhere to put an editor without home-manager, so leave the class
    # unclaimed and let the mount's assertion say so.
    lib.optionalAttrs (options ? home-manager) {
      rhizome.routed = [ "editor" ];

      home-manager.sharedModules = [
        { programs.editor.imports = config.rhizome.fragments.editor; }
      ];
    };
}
