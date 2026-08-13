pkgName:

# A `programs.<name>` module for packages upstream doesn't cover, where enable
# and package are the only options worth having.

{
  flake.modules.homeManager.platform =
    {
      config,
      lib,
      pkgs,
      ...
    }:

    let
      cfg = config.programs.${pkgName};
    in

    {
      options.programs.${pkgName} = {
        enable = lib.mkEnableOption "Whether to install ${pkgName}";
        package = lib.mkPackageOption pkgs pkgName { };
      };

      config.home.packages = lib.mkIf cfg.enable [ cfg.package ];
    };
}
