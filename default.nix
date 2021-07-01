{ config, pkgs, ... }:

# TODO: Replace `<unstable>` channel with a locked fetch or a flake.
let unstable = import <unstable> {};

in {
  # Install docker and run it automatically as a daemon.
  virtualisation.docker = {
    enable = true;
    package = unstable.docker;
    autoPrune.enable = true;
  };

  # Internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = "America/Los_Angeles";

  # Set system fonts.
  fonts = {
    enableDefaultFonts = true;
    fonts = [unstable.fira-code];
  };

  console = {
    font = "Fira Code";
    keyMap = "us";
  };

  # Misc hardware and drivers.
  services.printing.enable = true;
  sound.enable = true;
  hardware = {
    pulseaudio.enable = true;
    bluetooth.enable = true;
  };

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;

    xkbOptions = "caps:escape";
    autoRepeatDelay = 250;

    # Configure the touchpad.
    libinput = {
      enable = true;
      naturalScrolling = true;
      tapping = false; # Disable tap to click.
    };

    displayManager.lightdm = {
      greeters.enso.enable = true;
    };

    # Use XMonad to manage the graphical environment.
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };
  };

  # System programs.
  programs.wireshark.enable = true;
  programs.slock.enable = true; # Screen locking utility.

  programs.zsh = {
    enable = true;
    syntaxHighlighting.enable = true;
    autosuggestions.enable = true;
    histSize = 10000;

    shellAliases = {
      cat = "bat";
      ls = "exa";
      l = "exa -la";

      t = "tmux";
      ":qa" = "tmux kill-session 2> /dev/null || exit";

      g = "git";
      c = "git commit";
      b = "git branch";
      ch = "git checkout";
      h = "git diff HEAD";
      hh = "git diff HEAD~1";
      hhh = "git diff HEAD~2";

      ".." = "cd ..";
      "..." = "cd ../..";
      "...." = "cd ../../..";
    };
  };

  programs.tmux = {
    enable = true;
    keyMode = "vi";
    escapeTime = 0;
    historyLimit = 10000;
    customPaneNavigationAndResize = true;
    extraConfig = builtins.readFile ./config/tmux.conf;
  };

  # Set up the global environment.
  environment = {
    etc."zshrc.local".source = ./config/init.zsh;
    etc.gitconfig.source = ./config/git.ini;

    variables = {
      EDITOR = "nvim";
      MANPAGER = "nvim -c 'setfiletype man' -";
      SKIM_DEFAULT_COMMAND = "fd";
      BAT_THEME = "TwoDark";
      BAT_STYLE = "changes";

      # Provides dependencies for common Rust libraries.
      PKG_CONFIG_PATH = "${unstable.openssl.dev}/lib/pkgconfig";
    };

    systemPackages = with unstable; [
      rofi

      (callPackage ./pkgs/alacritty.nix {
        configFile = ./config/alacritty.yml;
      })

      # Editor. Installed globally to play nicely with sudo.
      (unstable.neovim.override {
        configure.customRC = builtins.readFile ./config/init.vim;

        configure.packages.plugins.start = with import <vim-plugins> { pkgs = unstable; }; [
          vim-plug
          ale
          auto-pairs
          onedark-vim
          rust-vim
          skim
          skim-vim
          vader-vim
          splitjoin-vim
          typescript-vim
          undotree
          vim-commentary
          vim-endwise
          vim-fugitive
          vim-gitgutter
          vim-graphql
          vim-markdown
          vim-nix
          vim-repeat
          vim-surround
          vim-swap
          vim-terraform
          vim-toml
          coc-nvim

          # 3rd party
          alternaut-vim
          teleport-vim
          navitron-vim
          further-vim
          vim-nand2tetris-syntax
          yajs-vim
          nginx-vim
          godown-vim

          # Nursery
          clippy-nvim
          git-vim
          misc-vim
          stacktrace-vim
        ];
      })
    ];
  };

  # Create a personal user profile.
  users.users.overlord = {
    isNormalUser = true;
    description = "Jesse Gibson";
    extraGroups = ["wheel" "docker"];
    shell = pkgs.zsh;

    packages = with unstable; [
      # Graphical Apps
      element-desktop
      torbrowser
      firefox
      zathura

      # Terminal Apps
      termshark
      ncspot
      aerc
      w3m

      # Rust Development
      rustup
      rustc
      cargo
      cargo-edit
      clippy
      rls
      gcc
      openssl.dev
      pkg-config

      # JS Development
      nodejs
      yarn

      # Infrastructure
      terraform_0_15
      ipmitool
      ansible
      kubectl

      # Tools
      gitAndTools.delta
      shellcheck
      miniserve
      playerctl
      binutils
      starship
      vim-vint
      ripgrep
      bottom
      dogdns
      zoxide
      pastel
      hexyl
      scrot
      tokei
      xclip
      acpi
      glow
      ipfs
      nmap
      rage
      skim
      exa
      bat
      git
      viu
      fd
      jq
      pv
      xh

      # Chat client
      (weechat.override {
        configure = { ... }: {
          scripts = with weechatScripts; [
            weechat-matrix
            wee-slack
          ];
        };
      })
    ];
  };
}
