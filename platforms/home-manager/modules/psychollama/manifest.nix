{ config, lib, ... }:

let
  inherit (lib) mkOption types;
in

{
  options.psychollama.manifest = mkOption {
    readOnly = true;
    type = types.listOf (
      types.submodule {
        options.path = mkOption {
          type = types.str;
          description = "Absolute target path on disk.";
        };
      }
    );

    description = ''
      All enabled `home.file` entries projected as `{ path }` records.
      Supports tooling for temporary editing without NixOS activation.
    '';

    default = lib.pipe config.home.file [
      (lib.filterAttrs (_: file: file.enable))
      lib.attrValues
      (map (file: {
        path = "${config.home.homeDirectory}/${file.target}";
      }))
    ];
  };
}
