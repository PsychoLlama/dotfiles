{ vimUtils }:

vimUtils.buildVimPlugin {
  pname = "ext.nvim";
  version = "latest";
  src = ./.;
}
