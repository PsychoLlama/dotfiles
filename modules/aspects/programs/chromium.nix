{
  exports.homeManager =
    {
      config,
      lib,
      pkgs,
      ...
    }:

    {
      programs.chromium = {
        enable = lib.mkDefault true;
        package = lib.mkDefault pkgs.unstable.chromium;
      };
    };
}
