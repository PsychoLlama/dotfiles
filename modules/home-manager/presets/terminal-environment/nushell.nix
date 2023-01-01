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

  nushell-env-file = {
    executable = true;
    source = ../../../../config/nushell/env.nu;
  };

in {
  options.presets.nushell.enable =
    mkEnableOption "Install and configure Nushell";

  config = mkIf cfg.enable {
    programs.nushell = {
      enable = true;
      package = pkgs.unstable.nushell;

      # Use the default aliases, except for `ls` overrides. Nushell has
      # a great `ls` replacement.
      shellAliases = filterAttrs (key: value: key != "l" && key != "ls")
        config.home.shellAliases // {
          l = "ls --all";
        };

      initExtra = ''
        source ${../../../../config/nushell/config.nu}
        source ${starship-prompt-setup}
        source ${zoxide-command-setup}
      '';
    };

    # TODO: Make this all configurable from outside the preset.
    xdg = optionalAttrs (pkgs.stdenv.isDarwin == false) {
      configFile."nushell/env.nu" = nushell-env-file;
    };

    # By default, HM symlinks config files to the wrong location on Darwin.
    # I do it manually to make it portable.
    home.file = optionalAttrs (pkgs.stdenv.isDarwin) {
      "Library/Application Support/nushell/env.nu" = nushell-env-file;
    };
  };
}
