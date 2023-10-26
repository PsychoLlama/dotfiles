{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.presets.nushell;
  starship-prompt-setup = pkgs.runCommand "starship-init" {
    buildInputs = [ pkgs.unstable.starship ];
  } ''
    HOME="$(mktemp --directory)" starship init nu > "$out"
  '';

  zoxide-command-setup =
    pkgs.runCommand "zoxide-init" { buildInputs = [ pkgs.unstable.zoxide ]; } ''
      zoxide init nushell > "$out"
      sed -e 's/&&/and/g' -e 's/||/or/g' --in-place "$out"
    '';

in {
  options.presets.nushell.enable =
    mkEnableOption "Install and configure Nushell";

  config = mkIf cfg.enable {
    programs.nushell = {
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
        source ${starship-prompt-setup}
        source ${zoxide-command-setup}
      '';

      extraEnv = ''
        source ${../../../../config/nushell/env.nu};
      '';
    };
  };
}
