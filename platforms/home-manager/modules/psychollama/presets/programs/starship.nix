{ config, lib, ... }:

let
  cfg = config.psychollama.presets.programs.starship;
in

{
  config.programs.starship = lib.mkIf cfg.enable {
    settings = {
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
