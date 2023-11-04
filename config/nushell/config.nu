$env.config = {
  edit_mode: vi
  shell_integration: true
  show_banner: false
  completions: {
    case_sensitive: false
    partial: false
    quick: true
    external: {
      enable: true
    }
  }
}

# Open the editor.
def n [...files: path] {
  if ($files | length) == 0 {
    nvim $env.PWD
  } else {
    nvim -p $files
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
def-env cf [] {
  let selected_dir = fd --type directory | fzf

  cd (if ($selected_dir | str length) > 0 {
    $selected_dir | str trim
  } else {
    ""
  })
}

# Show information about a nix package.
def gist [pkg_name: string] {
  let pkg = nix eval --offline --json $"nixpkgs#($pkg_name).meta" | from json

  # Probably because the package doesn't exist. Nix would've printed an error.
  if $pkg == null {
    return
  }

  let title = $"(ansi white_bold)($pkg.name) (ansi reset)($pkg.description)"
  let url = $"(ansi green)($pkg.homepage)(ansi reset)"

  $"($title)\n($url)"
}

# `mkdir` and `cd` in one move.
def-env md [directory: path] {
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

  nix shell (
    | [$package_name] ++ $extra_packages
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

  $plaintext | rage --armor $keys -
}

# Decrypt stdin using the SSH private key.
def decrypt [] {
  rage --decrypt --identity ~/.ssh/id_ed25519 -
}

# Manage git (p)rojects.
def-env p [
  project: string # A GitHub user/project shorthand.
  --root = ~/projects # Where to store all the repositories.
  --user = PsychoLlama # Your GitHub username.
  ...extra_git_args # Arguments passed on to `git clone`.
] {
  let resource = (
    [$"($user)/($project)"] ++ $project
    | parse '{user}/{repo}'
    | last
    | upsert user ($in.user | str downcase)
    | upsert repo ($in.repo | str replace '\b\.git$' '')
  )

  let clone_destination = [($root | path expand) $resource.user $resource.repo] | path join

  if not ($clone_destination | path exists) {
    git clone $"git@github.com:($resource.user)/($resource.repo)" $clone_destination $extra_git_args
  }

  cd $clone_destination
}

# Change the desktop wallpaper.
def 'wallpaper set' [image: path] {
  ln -sf $image ~/attic/images/wallpapers/current
  systemctl --user restart swaybg.service
}
