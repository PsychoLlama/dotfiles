# Dotfiles
A collection of spaghetti code masquerading as a dotfiles framework.

> **Warning:** this is highly tailored to my workflow and has tons of breaking
> changes almost weekly.<br />
  Don't actually use this. Feel free to browse and cannibalize the source, but
  please don't depend on the repo.

## What it does
After installation (`./setup`), there's a `dotfiles` command. It's the
entrypoint to the framework.

```txt
Usage: dotfiles <command>
    update  - Update the dotfiles framework
    link    - Symlink everything in 'dotfiles/linked'
    install - Install system-wide dependencies
    dir     - Print the dotfiles directory
    eject   - Gather historical files into a tarball
    unpack  - Restore ejected files back to their origin
```

Running `$ dotfiles update` pulls for changes, relinks all the config files
(`./linked/manifest.sh`), and ensures all the system dependencies have been
installed (`zsh`, `ag`, `python3`, `neovim`... see: `./install`).

It also manages quite possibly the most complex vimrc known to mankind
(`./editor`).

Random bash utils are strewn about the repo, but most of it's kept in
`./linked/.zshrc`.

#### dotfiles-env
Not everything can be published to GitHub, such as work-related utilities and
configurations, so there's a concept of environment extensions. The dotfiles
framework will check for `~/dotfiles-env` and use any zsh, tmux, or vim
settings declared there. Great for isolating company-related utilities into
separate repos.

#### dotfiles eject
Probably last thing to cover, since I do so much work in a VM, when it comes
time to switch environments I need to evacuate my ephemeral files. This
includes repl history, vim's persistent undo files, and directory history
(managed by [`z`](https://github.com/rupa/z)). I've automated this process
with `$ dotfiles eject`. It gathers up all those files and stuffs 'em into
a tarball. It's quite lovely.

## Making it yours
Nooooo, don't do that. This project is a giant dumpster of hacks. It does some
awesomely cool stuff and I love it, but it's a giant hack.

This is not a human-safe product.
