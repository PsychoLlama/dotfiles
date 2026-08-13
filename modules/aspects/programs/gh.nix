{
  exports.homeManager =
    {
      config,
      lib,
      pkgs,
      ...
    }:

    {
      programs.gh = {
        enable = lib.mkDefault true;
        package = pkgs.unstable.gh;

        settings = {
          git_protocol = "ssh";
        };
      };
    };
}
