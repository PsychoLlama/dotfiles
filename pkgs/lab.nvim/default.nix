{ vimUtils }:

vimUtils.buildVimPlugin {
  pname = "lab.nvim";
  version = "latest";
  src = ./.;
}
