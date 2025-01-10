# Manage git (p)rojects.
export def --env main [
  project_id: string # A GitHub user/project shorthand.
  --root: string = ~/projects # Where to store all the repositories.
  --user: string = PsychoLlama # Your GitHub username.
] {
  let project = $project_id | parse-project-id --default-user $user
  let clone_destination = [($root | path expand) ($project.user | str downcase) $project.repo] | path join

  if not ($clone_destination | path exists) {
    git clone $project.uri $clone_destination
  }

  cd $clone_destination
}

# Parse a repo address and return metadata.
export def parse-project-id [
  --default-user: string # A presumed username
]: string -> record {
  let project_id = $in

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
  let data = $project_id | parse '{user}/{repo}' | last
  if not ($data | is-empty) {
    return {
      host: 'github.com'
      user: $data.user
      repo: $data.repo
      uri: $"git@github.com:($data.user)/($data.repo).git"
    }
  }

  # "my-repo"
  return {
    host: 'github.com'
    user: $default_user
    repo: $project_id
    uri: $"git@github.com:($default_user)/($project_id).git"
  }
}
