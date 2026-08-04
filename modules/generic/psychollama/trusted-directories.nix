{ lib, ... }:

# Directory prefixes I trust across tools. A path at or beneath one of these
# is treated as owned and safe: the editor sources its project-local vimrc,
# direnv loads its `.envrc`, and Claude Code is granted filesystem access --
# all without prompting. Repos cloned beneath a trusted prefix (and any git
# worktrees under them) inherit the trust.
#
# Assigned per-host; defaults to trusting nothing. Stored in `~`-relative form;
# consumers that need absolute paths expand `~` to the home directory.

{
  options.psychollama.trusted-directories = lib.mkOption {
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
}
