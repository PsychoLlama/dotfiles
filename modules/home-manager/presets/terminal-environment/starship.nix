{ config, lib, pkgs, ... }:

let cfg = config.presets.starship;

in with lib; {
  options.presets.starship.enable =
    mkEnableOption "Use an opinionated Starship prompt";

  config.programs.starship = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.starship;

    settings = {
      add_newline = false;

      format = concatStrings [
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
