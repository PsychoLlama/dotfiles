{ dotfiles, ... }:

# Migrated counterpart to `psychollama.profiles.full`. Presets move here as
# they become aspects.

{
  dotfiles.profiles.full.includes = [
    dotfiles.programs.editor
    dotfiles.programs.git
  ];
}
