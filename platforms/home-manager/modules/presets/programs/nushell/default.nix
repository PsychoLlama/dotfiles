{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.presets.programs.nushell;
  jsonFormat = pkgs.formats.json { };

  zoxideCommandSetup = pkgs.runCommand "zoxide-init" { buildInputs = [ pkgs.unstable.zoxide ]; } ''
    zoxide init nushell > "$out"
    sed -e 's/-- $rest/-- ...$rest/' --in-place "$out"
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
      scripts = {
        enable = true;
        package = pkgs.unstable.nu_scripts;
        completions = [
          "cargo"
          "git"
          "nix"
          "npm"
        ];
      };

      # Use the default aliases, except for `ls` overrides. Nushell has
      # a great `ls` replacement.
      shellAliases = lib.filterAttrs (key: value: key != "l" && key != "ls") config.home.shellAliases // {
        l = "ls --all";
      };

      extraConfig = ''
        source ${./config.nu}
        source ${zoxideCommandSetup}

        open ${jsonFormat.generate "session-variables.json" safeSessionVariables} | load-env
      '';

      extraEnv = ''
        source ${./env.nu};
      '';
    };

    # The default completions are incompatible with newer versions of Nushell.
    zoxide.enableNushellIntegration = false;
  };
}
