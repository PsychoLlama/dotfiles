# Set up `nuenv` to execute trusted local `.env.nu` files if they exist.
use nu-hooks/nu-hooks/nuenv/hook.nu [
  "nuenv allow"
  "nuenv disallow"
]

$env.config.edit_mode = 'vi'
$env.config.show_banner = false
$env.config.footer_mode = 20
$env.config.filesize.unit = 'binary'
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

# Launch neovim.
def n [...files: path] {
  if ($files | length) == 0 {
    nvim $env.PWD
  } else {
    nvim -p ...$files
  }
}

# Launch emacs without a GUI.
def e [...files: path] {
  emacsclient --no-window-system ...(if ($files | is-empty) {
    [ ./ ]
  } else {
    $files
  })
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

# Search files with ripgrep and format results as structured data.
def --wrapped search [...args] {
  rg ...$args --json
  | from jsonl
  | where type == 'match'
  | get data
  | group-by path.text
  | transpose file matches
  | upsert matches {
      select lines.text line_number absolute_offset submatches
      | upsert submatches { select match.text start end | rename match }
      | rename content line column captures
      | upsert content { str trim }
    }
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

# Start a research session with read-only tool access.
def research [...args] {
  claude --agents (open --raw ~/.claude/dotfiles/share/agents.json) --agent researcher --permission-mode bypassPermissions ...$args
}

# Custom libraries
use dotfiles/repo *
use dotfiles/x *
