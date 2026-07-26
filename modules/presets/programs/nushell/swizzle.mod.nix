{ pkgs, ... }:

# Projects every enabled `home.file` entry as a JSON manifest, so the `swizzle`
# nushell command can temporarily replace a nix-managed file without a rebuild.
let
  json = pkgs.formats.json { };
in

{
  platforms.home-manager =
    { config, lib, ... }:

    let
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
