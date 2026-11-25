# Facts about the person a user scope belongs to.
{ rhizome, units, ... }:

let
  t = rhizome.types;
  inherit (units.platform) level;
in

{
  # Who they are, for anything that addresses or attributes them: the login,
  # git, group membership.
  identity = rhizome.channel "user.identity" {
    level = level.user;
    type.input = {
      username = t.str;
      name = t.str;
      email = t.str;
    };
    merge = rhizome.merge.one;
    optional = true;
  };

  # A directory prefix trusted across tools. At or beneath one, the editor
  # sources a project-local vimrc, direnv loads `.envrc`, and Claude Code is
  # granted filesystem access, all without prompting. `~`-relative; readers
  # that need absolute paths expand it.
  trusted-directory = rhizome.channel "user.trusted-directory" {
    level = level.user;
    type.input = t.str;
    merge = rhizome.merge.list;
    optional = true;
  };
}
