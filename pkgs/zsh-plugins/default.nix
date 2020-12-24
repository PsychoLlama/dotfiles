{ pkgs ? import <nixpkgs> {} }:

let exposeAsSharedModule = { src, name }: derivation {
  name = "zsh-plugin-${name}";
  system = builtins.currentSystem;
  builder = "${pkgs.bash}/bin/bash";
  args = [./builder.bash];
  pluginName = name;
  core = pkgs.coreutils;
  src = src;
};

in {
  inherit (pkgs) zsh-autosuggestions zsh-syntax-highlighting oh-my-zsh;

  prompt = exposeAsSharedModule {
    name = "llama.zsh-theme";
    src = pkgs.fetchFromGitHub {
      owner = "PsychoLlama";
      repo = "llama.zsh-theme";
      rev = "53c3ba8079378bc02b159aa7c6275ade1fddaa58";
      sha256 = "0n6vl0162yicnmnx20ng8bb6kgzkn1lh4xp5zl8040gmabzrc6s1";
    };
  };

  zsh-rustup-completion = exposeAsSharedModule {
    name = "zsh-rustup-completion";
    src = pkgs.fetchFromGitHub {
      owner = "pkulev";
      repo = "zsh-rustup-completion";
      rev = "bce8ad28197e5e34107cfcbd781fe9eafd86feaa";
      sha256 = "00w44np2vg86x5pss75fg0xp5m7c8rcrfj2l41231sqyqw762v1n";
    };
  };
}
