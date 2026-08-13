{
  exports.homeManager =
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
