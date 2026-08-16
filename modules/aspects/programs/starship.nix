{
  imports = [ (import ./_mk-unstable-preset.nix "starship") ];

  exports.homeManager =
    { lib, ... }:

    {
      programs.starship.settings = {
        "$schema" = "https://starship.rs/config-schema.json";

        add_newline = false;

        format = lib.concatStrings [
          "$directory"
          "$git_branch"
          "$git_state"
          "$git_status"
          "$character"
        ];

        directory = {
          format = "[$path](blue)";
          truncation_length = 1;
        };

        git_branch.format = "[\\[](yellow)[$branch](cyan)[\\]](yellow) ";

        git_state.format = "\\([$state( $progress_current/$progress_total)]($style)\\) ";

        git_status = {
          format = "([\\($all_status$ahead_behind\\)]($style) )";
          stashed = "";
        };

        character.format = "[$symbol]($style) ";
      };
    };
}
