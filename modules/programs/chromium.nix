{
  flake.modules.homeManager.default =
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
