{ cfg, lib, ... }:

# Directory prefixes I trust across tools. A path at or beneath one of these
# is treated as owned and safe: the editor sources its project-local vimrc,
# direnv loads its `.envrc`, and Claude Code is granted filesystem access --
# all without prompting. Repos cloned beneath a trusted prefix (and any git
# worktrees under them) inherit the trust.
#
# Assigned per-host; defaults to trusting nothing. Stored in `~`-relative form;
# consumers that need absolute paths expand `~` to the home directory.

{
  options = {
    # Data, not an effect. See `theme` for the reasoning.
    enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Whether to publish the list to platforms that still read `psychollama.trusted-directories`.";
    };

    paths = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      example = [ "~/projects" ];
      description = ''
        Directory prefixes trusted across tools (editor, direnv, Claude Code).
        Trusting a prefix once covers every repository and git worktree beneath
        it. `~` is expanded to the home directory where absolute paths are
        required.
      '';
    };
  };

  # Transitional. See `theme`. The editor inherits its copy from the
  # home-manager eval, so it needs no export of its own.
  platforms = {
    nixos.psychollama.trusted-directories = cfg.paths;
    home-manager.psychollama.trusted-directories = cfg.paths;
  };
}
