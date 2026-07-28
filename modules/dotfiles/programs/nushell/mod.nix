{
  cfg,
  lib,
  pkgs,
  ...
}:

let
  zoxideCommandSetup = pkgs.runCommand "zoxide-init" { buildInputs = [ pkgs.unstable.zoxide ]; } ''
    zoxide init nushell > "$out"
  '';
in

{
  # `abbreviations` and `libraries` sit here rather than on home-manager's
  # `programs.nushell` because peers contribute to them. An abbreviation is a
  # fact about git or tmux, not about the shell, and a peer writing it into
  # this namespace does not have to care whether nushell is the shell in play.
  options = {
    package = lib.mkPackageOption pkgs.unstable "nushell" { };

    abbreviations = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = { };
      description = ''
        Abbreviations expanded inline at the prompt. Unlike aliases, the
        expansion is committed to history, keeping substring search reliable.
      '';
      example = {
        gs = "git status";
        ll = "ls -l";
      };
    };

    libraries = lib.mkOption {
      type = lib.types.listOf (lib.types.either lib.types.str lib.types.path);
      default = [ ./libraries ];
      description = "Libraries visible in the search path.";
    };
  };

  # The swizzle manifest is only useful once nushell is installed, and it used
  # to share this preset's `enable`.
  config = {
    programs.nushell.swizzle.enable = lib.mkDefault true;
    programs.nushell.abbreviations.p = "project";
  };

  modules.home-manager =
    { config, ... }:

    let
      # Some modules use POSIX interpolation, which Nushell obviously doesn't
      # support. Just ignore them.
      safeSessionVariables = lib.filterAttrs (
        _: value: !(lib.isString value && lib.strings.hasInfix "\${" value)
      ) config.home.sessionVariables;
    in

    {
      programs.nushell = {
        enable = lib.mkDefault true;
        package = lib.mkDefault cfg.package;

        # Use the default aliases, except for `ls` overrides. Nushell has
        # a great `ls` replacement.
        shellAliases = lib.filterAttrs (key: value: key != "l" && key != "ls") config.home.shellAliases // {
          l = "ls --all";
        };

        extraConfig = ''
          source ${./config.nu}
          source ${zoxideCommandSetup}

          load-env ${config.lib.nushell.toNushell { } safeSessionVariables}
        ''
        + lib.optionalString (cfg.abbreviations != { }) ''

          $env.config.abbreviations = (
            $env.config.abbreviations | merge ${config.lib.nushell.toNushell { } cfg.abbreviations}
          )
        '';

        extraEnv = ''
          source ${./env.nu}

          ### Add custom libraries to the search path ###
          $env.NU_LIB_DIRS ++= ${config.lib.nushell.toNushell { } (lib.map toString cfg.libraries)}
        '';
      };

      # The default completions are incompatible with newer versions of Nushell.
      programs.zoxide.enableNushellIntegration = false;
    };
}
