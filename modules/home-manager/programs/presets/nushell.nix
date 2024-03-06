{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.presets.nushell;
  jsonFormat = pkgs.formats.json { };

  zoxideCommandSetup =
    pkgs.runCommand "zoxide-init" { buildInputs = [ pkgs.unstable.zoxide ]; } ''
      zoxide init nushell > "$out"
      sed -e 's/-- $rest/-- ...$rest/' --in-place "$out"
    '';

  # Some modules use POSIX interpolation, which Nushell obviously doesn't
  # support. Just ignore them.
  safeSessionVariables =
    filterAttrs (_: value: strings.hasInfix "\${" value == false)
    config.home.sessionVariables;

in {
  options.programs.presets.nushell.enable =
    mkEnableOption "Install and configure Nushell";

  config.programs = mkIf cfg.enable {
    nushell = {
      enable = true;
      package = pkgs.unstable.nushellFull;

      scripts = {
        enable = true;
        package = pkgs.unstable.nu_scripts;
        completions = [ "cargo" "git" "nix" "npm" ];
      };

      # Use the default aliases, except for `ls` overrides. Nushell has
      # a great `ls` replacement.
      shellAliases = filterAttrs (key: value: key != "l" && key != "ls")
        config.home.shellAliases // {
          l = "ls --all";
        };

      extraConfig = ''
        source ${../../../../config/nushell/config.nu}
        source ${zoxideCommandSetup}

        open ${
          jsonFormat.generate "session-variables.json" safeSessionVariables
        } | load-env

        ${optionalString pkgs.stdenv.isLinux ''
          def ip [...args] {
            ^ip --json $args | from json
          }
        ''}
      '';

      extraEnv = ''
        source ${../../../../config/nushell/env.nu};
      '';
    };

    # The default completions are incompatible with newer versions of Nushell.
    zoxide.enableNushellIntegration = false;
  };
}
