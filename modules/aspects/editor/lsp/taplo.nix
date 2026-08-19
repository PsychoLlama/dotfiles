{
  exports.editor =
    {
      lib,
      config,
      pkgs,
      ...
    }:

    let
      cfg = config.psychollama.presets.lsp.servers.taplo;
    in

    {
      options.psychollama.presets.lsp.servers.taplo = {
        package = lib.mkPackageOption pkgs.unstable "taplo" { };
      };

      config.lsp.servers.taplo = {
        cmd = [
          "${cfg.package}/bin/taplo"
          "lsp"
          "stdio"
        ];
        filetypes = [ "toml" ];
        root_markers = [
          ".taplo.toml"
          "taplo.toml"
          ".git/"
        ];
      };
    };
}
