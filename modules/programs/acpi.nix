{
  imports = [
    (import ./_mk-unstable-preset.nix "acpi")
    ../extensions/programs/acpi.nix
  ];
}
