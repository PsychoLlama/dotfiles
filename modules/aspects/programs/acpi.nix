{
  imports = [
    (import ./_mk-unstable-preset.nix "acpi")
    ../../platform/homeManager/programs/acpi.nix
  ];
}
