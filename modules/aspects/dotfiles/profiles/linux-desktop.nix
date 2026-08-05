{ dotfiles, ... }:

# Migrated counterpart to `psychollama.profiles.linux-desktop`. Presets move
# here as they become aspects.

{
  dotfiles.profiles.linux-desktop.includes = [
    dotfiles.fonts
    dotfiles.programs.fuzzel
    dotfiles.programs.swaylock
    dotfiles.programs.sway
  ];
}
