let
  # User SSH public key (for encrypting secrets)
  overlord = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHAMADENOb8Pe0kysfLc6BxK2VUiPMt57IOaDYa7J/M5";

  # Host SSH public keys (for decryption at runtime)
  # Get with: cat /etc/ssh/ssh_host_agenix_key.pub
  ava = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFlUATv/+bsebVVXQG+/ZQdXCJyRrrPUmZyOGM05HPss";

  allUsers = [ overlord ];
  allHosts = [ ava ];
in

{
  "platforms/nixos/modules/psychollama/presets/services/restic/env.age".publicKeys = allUsers ++ allHosts;
}
