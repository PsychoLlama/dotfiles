{
  imports = [ (import ./_mk-unstable-preset.nix "bat") ];

  exports.homeManager = {
    home.shellAliases.cat = "bat";

    programs.bat.config = {
      theme = "TwoDark";
      style = "changes";
    };
  };
}
