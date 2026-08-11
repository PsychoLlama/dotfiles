{
  flake.modules.homeManager.default =
    {
      config,
      lib,
      pkgs,
      ...
    }:

    {
      home.packages = [ pkgs.unstable.spotify ];
    };
}
