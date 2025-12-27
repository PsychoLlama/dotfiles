let
  # User SSH public key (for encrypting secrets)
  overlord = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHAMADENOb8Pe0kysfLc6BxK2VUiPMt57IOaDYa7J/M5";

  # Host SSH public keys (for decryption at runtime)
  # Get with: cat /etc/ssh/ssh_host_agenix_key.pub
  ava = "ssh-ed25519 ..."; # TODO: Replace after deploy

  allUsers = [ overlord ];
  allHosts = [ ava ];
in

{
  "restic-env.age".publicKeys = allUsers ++ allHosts;
}
