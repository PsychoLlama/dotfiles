{ self, ... }:

{
  # Used to initialize the user profile/home directory.
  dotfiles.user = {
    account = "ealderson";
    fullName = "Elliot Alderson";
  };

  # ... put your nixos config below ...
}
