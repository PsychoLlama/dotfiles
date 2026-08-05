{
  # Entity data only. The aspects behind these names live in `aspects/`.
  den.hosts.x86_64-linux.ava = {
    users.overlord = {
      # `user` manages the OS account, `homeManager` the home directory.
      classes = [
        "user"
        "homeManager"
      ];

      identity = {
        name = "Jesse Gibson";
        email = "JesseTheGibson@gmail.com";
      };
    };
  };
}
