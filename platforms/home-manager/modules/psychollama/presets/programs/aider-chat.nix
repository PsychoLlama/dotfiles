{
  pkgs,
  lib,
  config,
  ...
}:

let
  cfg = config.psychollama.presets.programs.aider-chat;
in

{
  options.psychollama.presets.programs.aider-chat.enable =
    lib.mkEnableOption "Install the latest version of aider-chat";

  config.programs.aider-chat = lib.mkIf cfg.enable {
    enable = lib.mkDefault true;
    package = lib.mkDefault pkgs.unstable.aider-chat;
    settings = {
      analytics-disable = true;
      check-update = false;
      code-theme = "one-dark";
      disable-playwright = true;
      gitignore = false;
      model = "gemini";
      restore-chat-history = true;
      vim = true;
    };
  };
}
