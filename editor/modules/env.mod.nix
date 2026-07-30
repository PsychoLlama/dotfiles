{ lib, ... }:

{
  options.env.trusted = lib.mkOption {
    type = lib.types.listOf lib.types.str;
    default = [ ];
    example = [ "~/projects" ];
    description = ''
      Directories trusted to source project-local vimrc files (direnv)
      without prompting. Any `DIRENV_EXTRA_VIMRC` resolving to a path at or
      beneath one of these directories is sourced automatically. `~` is
      expanded at runtime.

      Useful for a parent directory of repositories or git worktrees you
      already trust -- trusting the prefix once covers every checkout beneath
      it.
    '';
  };
}
