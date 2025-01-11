{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.programs.tmux;
  tmux = config.programs.tmux.package;
in

{
  config = lib.mkIf cfg.enable {
    home.shellAliases.t = "tmux";

    programs.tmux = {
      customPaneNavigationAndResize = true;
      escapeTime = 0;
      historyLimit = 100000;
      keyMode = "vi";
      shell = "${config.programs.nushell.package}/bin/nu";
      extraConfig = ''
        ${builtins.readFile ./tmux.conf}

        bind-key C-s display-popup -E ${pkgs.writers.writeBash "tmux-jump" ''
          sessions="$(${tmux}/bin/tmux list-sessions -F "#{session_name}")"
          session_name="$(echo -e "$sessions" | ${pkgs.fzf}/bin/fzf)"

          if [[ -n "$session_name" ]]; then
            ${tmux}/bin/tmux switch-client -t "$session_name"
          fi
        ''}
      '';
    };

    # The default session variable attempts POSIX interpolation, which
    # obviously doesn't work in Nushell.
    programs.nushell.extraEnv =
      if pkgs.stdenv.isDarwin then
        ''
          $env.TMUX_TMPDIR = "/tmp"
        ''
      else
        ''
          $env.TMUX_TMPDIR = $env.XDG_RUNTIME_DIR
        '';
  };
}
