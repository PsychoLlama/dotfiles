{
  imports = [ (import ./_mk-unstable-preset.nix "bat") ];

  flake.modules.homeManager.default = {
    home.shellAliases.cat = "bat";

    programs.bat.config = {
      theme = "TwoDark";
      style = "changes";
    };
  };
}
