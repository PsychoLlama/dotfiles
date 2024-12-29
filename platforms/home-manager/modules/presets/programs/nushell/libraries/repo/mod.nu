# Manage git (p)rojects.
export def --env main [
  project: string # A GitHub user/project shorthand.
  --root: string = ~/projects # Where to store all the repositories.
  --user: string = PsychoLlama # Your GitHub username.
] {
  let resource = (
    [$"($user)/($project)"] ++ [$project]
    | parse '{user}/{repo}'
    | last
    | upsert user ($in.user | str downcase)
    | upsert repo ($in.repo | str replace '\b\.git$' '')
  )

  let clone_destination = [($root | path expand) $resource.user $resource.repo] | path join

  if not ($clone_destination | path exists) {
    git clone $"git@github.com:($resource.user)/($resource.repo)" $clone_destination
  }

  cd $clone_destination
}
