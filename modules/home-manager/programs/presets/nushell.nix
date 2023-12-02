{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.presets.nushell;

  zoxide-command-setup =
    pkgs.runCommand "zoxide-init" { buildInputs = [ pkgs.unstable.zoxide ]; } ''
      zoxide init nushell > "$out"
      sed -e 's/def-env/def --env/g' --in-place "$out"
    '';

in {
  options.programs.presets.nushell.enable =
    mkEnableOption "Install and configure Nushell";

  config.programs = mkIf cfg.enable {
    nushell = {
      enable = true;
      package = pkgs.unstable.nushell;

      scripts = {
        enable = true;
        package = pkgs.unstable.nu_scripts;
        completions = [ "cargo" "git" "just" "nix" "npm" ];
      };

      # Use the default aliases, except for `ls` overrides. Nushell has
      # a great `ls` replacement.
      shellAliases = filterAttrs (key: value: key != "l" && key != "ls")
        config.home.shellAliases // {
          l = "ls --all";
        };

      extraConfig = ''
        source ${../../../../config/nushell/config.nu}
        source ${zoxide-command-setup}

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
