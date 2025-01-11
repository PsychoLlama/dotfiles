# Manage git (p)rojects.
export def --env main [
  project_id: string # A GitHub user/project shorthand.
] {
  let project = expr $project_id
  let clone_destination = dir $project

  if not ($clone_destination | path exists) {
    git clone $project.uri $clone_destination
  }

  cd $clone_destination
}

# Determine where a project should exist on disk.
export def dir [
  project: record
] {
  [
    (project-root | path expand)
    ($project.user | str downcase)
    $project.repo
  ] | path join
}

# Get the root directory of all projects.
export def project-root [
  suggested_root?: string
] {
  $suggested_root | default ($env | get -i REPO_ROOT) | default ~/projects | path expand
}

# Parse a repo address and return metadata.
export def expr [
  project_id: string
] {
  # "https://github.com/github/docs/blob/main/file"
  if ($project_id | str starts-with "https://github.com") {
    let data = $project_id | parse --regex '^https:..github.com/(?<user>[^/]+)/(?<repo>[^/]+).*'
    let user = $data.user.0
    let repo = $data.repo.0
    return {
      host: 'github.com'
      user: $user
      repo: $repo
      uri: $"git@github.com:($user)/($repo).git"
    }
  }

  # "username/their-repo"
  let data = $project_id | parse '{user}/{repo}'
  if not ($data | is-empty) {
    let data = $data | first

    return {
      host: 'github.com'
      user: $data.user
      repo: $data.repo
      uri: $"git@github.com:($data.user)/($data.repo).git"
    }
  }

  error make {
    msg: "Could not parse project ID."
  }
}

# List all projects.
export def list [] {
  fd . --type d --max-depth 2 --min-depth 2 (project-root)
    | lines
    | each {
      path split
        | last 2
        | { user: $in.0, repo: $in.1 }
    }
}
