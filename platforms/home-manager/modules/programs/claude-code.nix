{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.claude-code;
  json = pkgs.formats.json { };
in

{
  options.programs.claude-code = {
    enable = lib.mkEnableOption "Whether to install the Claude Code CLI";
    package = lib.mkPackageOption pkgs "claude-code" { };
    settings = lib.mkOption {
      type = json.type;
      default = { };
      description = "Global settings for Claude Code";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];
    home.file.".claude/settings.json" = lib.mkIf (cfg.settings != { }) {
      source = json.generate "claude-settings.json" cfg.settings;
    };
  };
}
