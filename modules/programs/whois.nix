{
  imports = [
    (import ./_mk-unstable-preset.nix "whois")
    ../extensions/programs/whois.nix
  ];
}
