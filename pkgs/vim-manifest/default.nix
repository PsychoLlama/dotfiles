with (import ./vim-manifest.nix);

manifest [
  # Filetype plugins
  ["davinche/godown-vim" { for = "markdown"; }]
  "leafgarland/typescript-vim"
  "PsychoLlama/debrief.vim"
  "hashivim/vim-terraform"
  "jparise/vim-graphql"
  "rust-lang/rust.vim"
  "tpope/vim-markdown"
  "cespare/vim-toml"
  "othree/yajs.vim"
  "chr4/nginx.vim"
  "LnL7/vim-nix"
  "mxw/vim-jsx"


  # ALL HAIL TIM POPE
  "tpope/vim-commentary"
  "tpope/vim-fugitive"
  "tpope/vim-surround"
  "tpope/vim-endwise"
  "tpope/vim-repeat"

  # Language Server Protocol client.
  (import ./coc-nvim/default.nix)

  # Enhancements ✨
  "AndrewRadev/splitjoin.vim"
  "jiangmiao/auto-pairs"
  "machakann/vim-swap"
  "mbbill/undotree"
  "w0rp/ale"

  # Navigation
  "PsychoLlama/alternaut.vim"
  "PsychoLlama/navitron.vim"
  "PsychoLlama/teleport.vim"
  "PsychoLlama/further.vim"
  "lotabout/skim.vim"
  "lotabout/skim"

  # Style 🦄
  "airblade/vim-gitgutter"
  "joshdick/onedark.vim"

  # Fun 🎮
  "PsychoLlama/conway.vim"
  "PsychoLlama/snake.vim"
]
