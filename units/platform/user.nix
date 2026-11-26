# A person on the machine, and facts about them.
{ rhizome, units, ... }:

let
  t = rhizome.types;
  inherit (units.platform) home-manager host;
in

rhizome.unit "user" (user: {
  level = rhizome.level { within = host.level; };

  # Who they are, for anything that addresses or attributes them: the login,
  # git, group membership.
  identity = rhizome.channel {
    level = user.level;
    merge = rhizome.merge.one;
    optional = true;

    type = {
      username = t.str;
      name = t.str;
      email = t.str;
    };
  };

  # A directory prefix trusted across tools. At or beneath one, the editor
  # sources a project-local vimrc, direnv loads `.envrc`, and Claude Code is
  # granted filesystem access, all without prompting. `~`-relative; readers
  # that need absolute paths expand it.
  trusted-directory = rhizome.channel {
    level = user.level;
    type = t.str;
    merge = rhizome.merge.list;
    optional = true;
  };

  scope = user.level.scope (cfg: {
    label = cfg.username;
    type = user.identity.type;

    config = [
      (user.identity cfg)
      home-manager.unit
    ];
  });
})
