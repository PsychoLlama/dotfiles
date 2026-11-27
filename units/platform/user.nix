# A person on the machine, and facts about them.
{
  rhizome,
  units,
  self,
  define,
  ...
}:

let
  t = rhizome.types;
  inherit (units.platform) home-manager host;
in

{
  level = define.level { within = host.level; };

  # Who they are, for anything that addresses or attributes them: the login,
  # git, group membership.
  identity = define.fact {
    level = self.level;
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
  trusted-directory = define.fact {
    level = self.level;
    type = t.str;
    merge = rhizome.merge.list;
    optional = true;
  };

  instance = self.level.instance (cfg: {
    label = cfg.username;
    input = self.identity.type;

    config = [
      (self.identity cfg)
      home-manager.unit
    ];
  });
}
