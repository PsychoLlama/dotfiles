{
  flake.modules.homeManager.default =
    {
      config,
      lib,
      pkgs,
      ...
    }:

    let
      cfg = config.psychollama.presets.programs.nushell;
      json = pkgs.formats.json { };

      # All enabled `home.file` entries projected as `{ path }` records. Supports
      # tooling for temporary editing without NixOS activation.
      manifest = lib.pipe config.home.file [
        (lib.filterAttrs (_: file: file.enable))
        lib.attrValues
        (map (file: {
          path = "${config.home.homeDirectory}/${file.target}";
        }))
      ];
    in

    {
      config = lib.mkIf cfg.enable {
        home.file.".config/swizzle/manifest.json".source = json.generate "swizzle-manifest.json" manifest;
      };
    };
}
