{ lib, pkgs, ... }:

{
  platforms.home-manager.programs.starship = {
    enable = lib.mkDefault true;
    package = lib.mkDefault pkgs.unstable.starship;

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
