{
  flake.modules.homeManager.default =
    {
      config,
      lib,
      pkgs,
      ...
    }:

    {
      services.ssh-agent = {
        enable = true;
        package = pkgs.unstable.openssh;
      };
    };
}
