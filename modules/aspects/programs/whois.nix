{
  imports = [
    (import ./_mk-unstable-preset.nix "whois")
    ../../platform/homeManager/programs/whois.nix
  ];
}
