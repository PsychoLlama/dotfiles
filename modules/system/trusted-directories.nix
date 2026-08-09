{ lib, ... }:

# Directory prefixes I trust across tools. A path at or beneath one of these
# is treated as owned and safe: the editor sources its project-local vimrc,
# direnv loads its `.envrc`, and Claude Code is granted filesystem access --
# all without prompting. Repos cloned beneath a trusted prefix (and any git
# worktrees under them) inherit the trust.
#
# Defaults to trusting nothing. Stored in `~`-relative form; consumers that need
# absolute paths expand `~` to the home directory.
#
# Declared on the flake rather than as a `generic` module. Trust follows the
# person, not the machine, and every class that might read it had to be handed a
# copy -- `substrate.nix` replayed the nixos value into home-manager and again
# into the editor submodule. One flake option is read directly by every consumer,
# including the editor, which evaluates in its own module system.

{
  options.trusted-directories = lib.mkOption {
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
