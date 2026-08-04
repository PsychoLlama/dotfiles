{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.git.fsmonitor;

  # Git ships a reference watchman hook for fsmonitor protocol v2 (perl, v2
  # tokens). Upstream git has no Linux fsmonitor daemon, so we reuse the hook
  # and delegate watching to watchman (inotify/fanotify under the hood).
  sampleHook = "${config.programs.git.package}/share/git-core/templates/hooks/fsmonitor-watchman.sample";
in

{
  options.programs.git.fsmonitor = {
    enable = lib.mkEnableOption "watchman-backed git fsmonitor on Linux";

    package = lib.mkPackageOption pkgs "watchman" { };

    watchScript = lib.mkOption {
      type = lib.types.package;
      readOnly = true;
      description = "Script that opts the current repo into watchman fsmonitor.";
      default = pkgs.writeShellApplication {
        name = "git-fsmonitor-watch";
        runtimeInputs = [
          config.programs.git.package
          cfg.package
        ];
        text = ''
          git config core.fsmonitor ${sampleHook}
          git update-index --fsmonitor
        '';
      };
    };

    unwatchScript = lib.mkOption {
      type = lib.types.package;
      readOnly = true;
      description = "Script that opts the current repo out of watchman fsmonitor.";
      default = pkgs.writeShellApplication {
        name = "git-fsmonitor-unwatch";
        runtimeInputs = [ config.programs.git.package ];
        text = ''
          git config --unset core.fsmonitor
          git update-index --no-fsmonitor
        '';
      };
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];
  };
}
