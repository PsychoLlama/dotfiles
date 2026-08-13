{
  exports.homeManager =
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
