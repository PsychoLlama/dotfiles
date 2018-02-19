# Dotfiles
You know what this is.

## Usage
Clone this repo, then run `./setup.sh`. You may need `sudo`.

```
Usage: dotfiles [command]
    link    - Symlink everything in 'dotfiles/linked'
    update  - Pull dotfile changes from git
    install - Install system-wide dependencies
    dir     - Print the dotfiles directory
```

Now you've got a `dotfiles` command. It manages updates, symlinking, and global dependency management. If something is added to the symlink list or dependency manager, it'll be run the next time `update` is called.

## Making it yours
Don't. I made this framework for me. You could probably adapt it, but I'd recommend just building your own or forking someone else's. It's far more satisfying and makes you look cool :sunglasses:
