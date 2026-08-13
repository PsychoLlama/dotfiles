{ lib, ... }:

# Not a flake module. This extends `rhizome.hosts.<name>`, imported by a host
# that wants it. Underscore-prefixed so the sweep skips it.
#
# Directory prefixes I trust across tools. A path at or beneath one of these
# is treated as owned and safe: the editor sources its project-local vimrc,
# direnv loads its `.envrc`, and Claude Code is granted filesystem access --
# all without prompting. Repos cloned beneath a trusted prefix (and any git
# worktrees under them) inherit the trust.
#
# Defaults to trusting nothing. Stored in `~`-relative form; consumers that need
# absolute paths expand `~` to the home directory.
#
# A host option, following `identity`: trust follows the person, and the person
# is now named per machine. Every class that might read it once had to be handed
# a copy -- `substrate.nix` replayed the nixos value into home-manager and again
# into the editor submodule -- but `host` reaches all three.

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
