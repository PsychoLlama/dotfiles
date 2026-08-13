{
  imports = [ (import ./_mk-unstable-preset.nix "delta") ];

  exports.homeManager.programs.delta = {
    enableGitIntegration = true;

    options = {
      dark = true;
      syntax-theme = "OneHalfDark";
    };
  };
}
