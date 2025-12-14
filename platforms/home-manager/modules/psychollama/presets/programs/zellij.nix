{
  config,
  lib,
  ...
}:

let
  cfg = config.psychollama.presets.programs.zellij;
  nu = config.programs.nushell.package;
in

{
  options.psychollama.presets.programs.zellij = {
    enable = lib.mkEnableOption "Opinionated Zellij terminal multiplexer config";
  };

  config = lib.mkIf cfg.enable {
    programs.zellij = {
      enable = true;

      settings = {
        default_shell = "${nu}/bin/nu";
        default_layout = "minimal";

        # Disable pane borders and powerline styling
        simplified_ui = true;
        pane_frames = false;

        # Match tmux scrollback (100k lines)
        scroll_buffer_size = 100000;

        # OSC 52 clipboard (like tmux set-clipboard on)
        copy_clipboard = "system";
        copy_on_select = true;

        # Disable startup banners
        show_release_notes = false;
        show_startup_tips = false;
      };

      # Minimal layout: no status bar, no tab bar
      layouts.minimal = ''
        layout {
          pane
        }
      '';
    };
  };
}
