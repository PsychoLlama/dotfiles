# Config Reference

A generated reference for all configs available in my dotfiles.

Configs are groups of **presets** and **profiles**. They're grouped by namespace, so `home-manager` and `nixos` get separate modules.

```nix
modules = [
  dotfiles.nixosModules.editor-platform
  dotfiles.nixosModules.editor-configs
]
```

Each preset can be enabled individually:

```nix
{
  # Enable the JSON language server.
  config.psychollama.presets.lsp.servers.jsonls.enable = true;
}
```

Or in bulk:

```nix
{
  # Adopt my entire editor config.
  config.psychollama.profiles.full.enable = true;
}
```

## Backwards Compatibility

None.

This documentation doesn't exist to support users. It exists because I wanted to see if I could generate it.
