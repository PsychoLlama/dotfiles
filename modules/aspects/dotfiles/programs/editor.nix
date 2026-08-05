{
  dotfiles.programs.editor = {
    homeManager =
      { config, ... }:

      let
        inherit (config.programs.editor) neovim;
      in

      {
        home.sessionVariables = {
          EDITOR = "${neovim}/bin/nvim";
          MANPAGER = "${neovim}/bin/nvim -c 'Man!'";
        };

        programs.git.ignores = [ ".vimrc.lua" ];
      };

    # `package` is left to the editor's own `full` profile. Priority markers
    # don't survive the class route, so a `mkDefault` here would collide.
    editor = {
      enable = true;
      psychollama.profiles.full.enable = true;
    };
  };
}
