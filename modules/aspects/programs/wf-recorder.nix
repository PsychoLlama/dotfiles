{
  exports.homeManager =
    { lib, pkgs, ... }:

    {
      programs.wf-recorder = {
        enable = lib.mkDefault true;
        # Same version as unstable, which currently fails to build (ffmpeg 9 dropped `AVCodec.sample_fmts`).
        package = lib.mkDefault pkgs.wf-recorder;
      };
    };
}
