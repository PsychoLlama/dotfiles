{ config, ... }:

# Bound out here because the module below shadows `config` with its own.
let
  inherit (config) trusted-directories;
in

{
  imports = [ ../../system/trusted-directories.nix ];

  flake.modules.homeManager.default =
    {
      config,
      lib,
      pkgs,
      ...
    }:

    let
      # direnv needs absolute prefixes; expand a leading `~` to the home directory.
      toAbsolute =
        dir:
        if lib.hasPrefix "~/" dir then "${config.home.homeDirectory}/${lib.removePrefix "~/" dir}" else dir;
    in

    {
      programs.direnv = {
        enable = lib.mkDefault true;
        package = lib.mkDefault pkgs.unstable.direnv;

        enableNushellIntegration = false;
        nix-direnv.enable = true;

        config = {
          global.hide_env_diff = true;

          whitelist.prefix = map toAbsolute trusted-directories;
        };
      };

      # Forked from home-manager. Nushell removed the `let-env` command.
      programs.nushell.extraConfig = lib.mkAfter ''
        $env.config = ($env | default {} config).config
        $env.config = ($env.config | default {} hooks)
        $env.config = ($env.config | update hooks ($env.config.hooks | default [] pre_prompt))
        $env.config = ($env.config | update hooks.pre_prompt ($env.config.hooks.pre_prompt | append {
          code: "
            let direnv = (${pkgs.direnv}/bin/direnv export json | from json)
            let direnv = if $direnv == null { {} } else { $direnv }
            $direnv | load-env
          "
        }))
      '';

      programs.git.ignores = [
        # No trailing slash so it also matches the bare symlink worktrees use.
        ".direnv"
        ".envrc"
      ];
    };
}
