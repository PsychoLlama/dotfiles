{
  imports = [ (import ./_mk-unstable-preset.nix "starship") ];

  flake.modules.homeManager.default =
    { lib, ... }:

    {
      programs.starship.settings = {
        add_newline = false;

        format = lib.concatStrings [
          "$directory"
          "$git_branch"
          "$git_status"
          "$git_state "
          "$character"
        ];

        directory = {
          format = "[$path](blue)";
          truncation_length = 1;
        };

        git_branch.format = "[\\[](yellow)[$branch](cyan)[\\]](yellow)";
      };
    };
}
