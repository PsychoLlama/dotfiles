{
  lib,
  config,
  pkgs,
  ...
}:

let
  cfg = config.programs.aider-chat;
  yaml = pkgs.formats.yaml { };
in

{
  options.programs.aider-chat = {
    enable = lib.mkEnableOption "Install aider";
    package = lib.mkPackageOption pkgs "aider-chat" { };
    settings = lib.mkOption {
      type = yaml.type;
      default = { };
      description = ''
        Global aider configuration file.
        See: https://aider.chat/docs/config/aider_conf.html
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = lib.mkIf cfg.enable [ cfg.package ];
    home.file.".aider.conf.yml".source = lib.mkIf (cfg.settings != { }) (
      yaml.generate ".aider.conf.yml" cfg.settings
    );
  };
}
