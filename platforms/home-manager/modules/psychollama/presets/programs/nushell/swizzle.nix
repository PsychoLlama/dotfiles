{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.programs.nushell;
in

{
  config = lib.mkIf cfg.enable {
    home.file.".config/swizzle/manifest.json".source = pkgs.writeText "swizzle-manifest.json" (
      builtins.toJSON config.psychollama.manifest
    );
  };
}
