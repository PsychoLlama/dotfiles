{
  flake.modules.homeManager.default =
    {
      config,
      lib,
      pkgs,
      ...
    }:

    let
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
      home.file.".config/swizzle/manifest.json".source = json.generate "swizzle-manifest.json" manifest;
    };
}
