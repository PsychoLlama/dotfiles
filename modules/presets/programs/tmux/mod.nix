{ lib, pkgs, ... }:

{
  platforms.home-manager =
    { config, ... }:

    let
      tmux = lib.getExe' config.programs.tmux.package "tmux";
      nu = lib.getExe' config.programs.nushell.package "nu";
    in

    {
      programs.nushell.abbreviations.t = "tmux";

      programs.tmux = {
        enable = lib.mkDefault true;
        package = lib.mkDefault pkgs.unstable.tmux;

        customPaneNavigationAndResize = true;
        escapeTime = 0;
        historyLimit = 100000;
        keyMode = "vi";
        shell = nu;
        extraConfig = ''
          ${builtins.readFile ./tmux.conf}

          bind-key C-s display-popup -E ${pkgs.writers.writeBash "tmux-jump" ''
            sessions="$(${tmux} list-sessions -F "#{session_name}")"
            session_name="$(echo -e "$sessions" | ${lib.getExe pkgs.fzf})"

            if [[ -n "$session_name" ]]; then
              ${tmux} switch-client -t "$session_name"
            fi
          ''}

          bind-key v display-popup -E ${pkgs.writers.writeBash "tmux-dictation" ''
            target_pane="$TMUX_PANE"
            text="$(dictation)"

            if [[ -n "$text" ]]; then
              ${tmux} send-keys -t "$target_pane" -- "$text"
            fi
          ''}
        '';
      };

      # The default session variable attempts POSIX interpolation, which
      # obviously doesn't work in Nushell.
      programs.nushell.extraEnv = ''
        $env.TMUX_TMPDIR = $env.XDG_RUNTIME_DIR
      '';
    };
}
