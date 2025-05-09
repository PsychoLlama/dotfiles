# Set up `nuenv` to execute trusted local `.env.nu` files if they exist.
use nu-hooks/nu-hooks/nuenv/hook.nu [
  "nuenv allow"
  "nuenv disallow"
]

$env.config.edit_mode = 'vi'
$env.config.show_banner = false
$env.config.footer_mode = 20
$env.config.history = {
  file_format: 'sqlite'
  max_size: 100_000_000_000
  isolation: true
}

$env.config.table = {
  mode: 'rounded'
  header_on_separator: true
}

$env.config.cursor_shape = {
  vi_normal: 'block'
  vi_insert: 'line'
}

$env.config.completions = {
  case_sensitive: false
  partial: false
  quick: true
  external: {
    enable: true
  }
}

$env.config.hooks.env_change = {
  PWD: [
    (use nu-hooks/nu-hooks/nuenv/hook.nu; hook setup)
  ]
}

# Open the editor.
def n [...files: path] {
  if ($files | length) == 0 {
    nvim $env.PWD
  } else {
    nvim -p ...$files
  }
}

# Open emacs.
def e [...files: path] {
  if ($files | length) == 0 {
    emacs -nw $env.PWD
  } else {
    emacs -nw ...$files
  }
}

# Fuzzy find and edit a file.
def nf [] {
  let selected_file = fd --type file | fzf

  if ($selected_file | str length) > 0 {
    nvim ($selected_file | str trim)
  }
}

# Fuzzy find and open a directory.
def nt [] {
  let selected_dir = fd --type directory | fzf

  if ($selected_dir | str length) > 0 {
    nvim ($selected_dir | str trim)
  }
}

# Fuzzy find and open a directory.
def --env cf [] {
  let selected_dir = fd --type directory | fzf

  cd (if ($selected_dir | str length) > 0 {
    $selected_dir | str trim
  } else {
    ""
  })
}

# Open an LLM chat session.
def chat [] {
  nvim -c 'execute "CodeCompanionChat" | wincmd l | quit'
}

# Show information about a nix package.
def gist [
  # Any attribute of `pkgs`
  pkg_path: string

  # Show all the metadata
  --long (-l)

  # Open the homepage
  --open (-o)
] {
  let pkg = nix eval --offline --json $"nixpkgs#($pkg_path).meta" | from json

  # Probably because the package doesn't exist. Nix would've printed an error.
  if $pkg == null {
    return
  }

  if $long {
    return $pkg
  }

  if $open {
    start $pkg.homepage
    return $pkg.homepage
  }

  $pkg
    | select name? description? homepage?
    | transpose key value
    | where value != null
    | reduce --fold {} { |row, acc| $acc | merge { $row.key: $row.value } }
}

# `mkdir` and `cd` in one move.
def --env md [directory: path] {
  mkdir $directory
  cd $directory
}

# Show git status.
def s [] {
  let repo_check = do --ignore-errors {
    git rev-parse --is-inside-work-tree | complete
  }

  if $repo_check.exit_code == 0 {
    git status
  } else {
    echo 'Not a git repo.'
  }
}

# Drop into a shell providing nix packages.
def "," [
  package_name: string # Anything from nixpkgs.
  ...extra_packages: string # Optional extras.
] {
  # Default bare identifiers to the nixpkgs flake.
  def canonicalize [ident: string] {
    if ($ident | str contains '#') {
      $ident
    } else {
      $"nixpkgs#($ident)"
    }
  }

  nix shell ...(
    | [$package_name ...$extra_packages]
    | each { |ident| canonicalize $ident }
  )
}

# Drop into a nix development shell.
def ">>" [] {
  nix develop --command $env.SHELL
}

# Encrypt stdin using public keys from GitHub.
def encrypt [
  username: string # Any GitHub username.
] {
  let plaintext = $in
  let keys = (
    | http get $"https://github.com/($username).keys"
    | lines
    | each { || [ "--recipient" $in ] }
    | flatten
  )

  $plaintext | rage --armor ...$keys -
}

# Decrypt stdin using the SSH private key.
def decrypt [] {
  rage --decrypt --identity ~/.ssh/id_ed25519 -
}

# Change the desktop wallpaper.
def 'wallpaper set' [image: path] {
  ln -sf $image ~/attic/images/wallpapers/current
  systemctl --user restart swaybg.service
}

# Custom libraries
use dotfiles/repo *
