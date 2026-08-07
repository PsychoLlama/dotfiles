{
  imports = [ (import ./_mk-unstable-preset.nix "delta") ];

  flake.modules.homeManager.default.programs.delta = {
    enableGitIntegration = true;

    options = {
      dark = true;
      syntax-theme = "OneHalfDark";
    };
  };
}
