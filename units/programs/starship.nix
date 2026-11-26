{ rhizome, units, ... }:

let
  inherit (units.platform) home-manager;
in

rhizome.unit "starship" {
  unit = rhizome.component [
    (home-manager.program "starship")

    (home-manager.module (
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
            format = "[$path](blue) ";
            truncation_length = 1;
          };

          git_branch = {
            format = "[$branch]($style) ";
            style = "none";
          };

          git_state.format = "\\([$state( $progress_current/$progress_total)]($style)\\) ";

          git_status = {
            format = "([\\($all_status$ahead_behind\\)]($style) )";
            stashed = "";
          };

          character.format = "[$symbol]($style) ";
        };
      }
    ))
  ];
}
