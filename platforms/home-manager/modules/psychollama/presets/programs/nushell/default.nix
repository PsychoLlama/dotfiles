{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.programs.nushell;

  zoxideCommandSetup = pkgs.runCommand "zoxide-init" { buildInputs = [ pkgs.unstable.zoxide ]; } ''
    zoxide init nushell > "$out"
  '';

  # Some modules use POSIX interpolation, which Nushell obviously doesn't
  # support. Just ignore them.
  safeSessionVariables = lib.filterAttrs (
    _: value: lib.strings.hasInfix "\${" value == false
  ) config.home.sessionVariables;
in

{
  config.programs = lib.mkIf cfg.enable {
    nushell = {
      libraries = {
        enable = true;
        path = [ ./libraries ];

        nu_scripts = {
          enable = true;
          package = pkgs.unstable.nu_scripts;

          completions = [
            "cargo"
            "git"
            "glow"
            "nix"
            "npm"
            "rg"
            "rustup"
            "ssh"
            "tar"
          ];
        };
      };

      # Use the default aliases, except for `ls` overrides. Nushell has
      # a great `ls` replacement.
      shellAliases = lib.filterAttrs (key: value: key != "l" && key != "ls") config.home.shellAliases // {
        l = "ls --all";
      };

      extraConfig = ''
        source ${./config.nu}
        source ${zoxideCommandSetup}

        load-env ${lib.hm.nushell.toNushell { } safeSessionVariables}
      '';

      extraEnv = ''
        source ${./env.nu}
      '';
    };

    # The default completions are incompatible with newer versions of Nushell.
    zoxide.enableNushellIntegration = false;
  };
}
