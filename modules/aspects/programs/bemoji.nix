{
  imports = [
    ../../platform/homeManager/programs/bemoji.nix
    ../../platform/homeManager/programs/wtype.nix
  ];

  flake.modules.homeManager.default =
    { pkgs, ... }:

    {
      programs.bemoji = {
        enable = true;
        package = pkgs.unstable.bemoji;
      };

      # bemoji uses wtype to type the selected emoji into the focused window.
      programs.wtype = {
        enable = true;
        package = pkgs.unstable.wtype;
      };

      home.sessionVariables.BEMOJI_PICKER_CMD = "fuzzel -d";
    };
}
