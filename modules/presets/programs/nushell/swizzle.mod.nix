{ pkgs, ... }:

# Projects every enabled `home.file` entry as a JSON manifest, so the `swizzle`
# nushell command can temporarily replace a nix-managed file without a rebuild.
{
  platforms.home-manager =
    { config, ... }:

    {
      home.file.".config/swizzle/manifest.json".source = pkgs.writeText "swizzle-manifest.json" (
        builtins.toJSON config.psychollama.manifest
      );
    };
}
