{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.presets.nushell;
  starship-prompt-setup = pkgs.runCommand "starship-init" {
    buildInputs = [ pkgs.unstable.starship ];
  } ''
    HOME="$(mktemp --directory)" starship init nu > "$out"
    sed 's/term size -c/term size/' --in-place "$out"
  '';

  zoxide-command-setup =
    pkgs.runCommand "zoxide-init" { buildInputs = [ pkgs.unstable.zoxide ]; } ''
      zoxide init nushell > "$out"
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
          l = "ls";
        };

      initExtra = ''
        source ${../../../../config/nushell/config.nu}
        source ${starship-prompt-setup}
        source ${zoxide-command-setup}
      '';

      # By default, this symlinks to the wrong location with the wrong format.
      # Use nulang config files instead.
      settings = { };
    };

    # TODO: Make this all configurable from outside the preset.
    xdg = optionalAttrs (pkgs.stdenv.isDarwin == false) {
      configFile."nushell/env.nu" = nushell-env-file;
    };

    # Why they gotta make it so hard.
    home.file = optionalAttrs (pkgs.stdenv.isDarwin) {
      "Library/Application Support/nushell/env.nu" = nushell-env-file;
    };
  };
}
