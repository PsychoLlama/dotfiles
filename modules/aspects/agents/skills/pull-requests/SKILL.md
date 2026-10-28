## Rules

- Don't create a PR unless asked.
- Only merge if explicitly instructed.
- NEVER open a PR or issue against another repo. Only the current repo's `origin`.
- NEVER link foreign issues/PRs. Only link entities belonging to the repo.
- NEVER tag users.
- Keep titles/descriptions up to date when pushing new commits.

## Process

1. The project's checks must pass before opening a PR.
2. Start the PR as draft.
3. Tell me `PR: <url>` as soon as it's open.
4. Iterate against CI using `gh`. Watch in a background task.
5. Mark ready for review when CI passes.

## Merging

1. Immediately mark as ready for review.
2. Use `gh pr merge <pr> --auto --merge`.
3. Wait for PR to merge.
4. Stay on your branch. Fetch and `rebase --autostash` latest main.

### Notes

- Avoid squash commits. Prefer merge or rebase.
- Merging automatically deletes the upstream branch.

## PR Format

### Title

- Must start with a robot face emoji: 🤖

### Body

- Must use soft-wrapped text.
- Never include a test plan section.

### Template

```
## TL;DR:

<!-- Problem context or goal. 1-3 sentences max. -->

**Changes**

<!-- List. 1 sentence each. -->

<!-- ... [optional] other header + text sections ... -->

## Pros

<!-- List. 1 sentence each. -->

## Cons

<!-- List. 1 sentence each. -->

## Followup

<!-- List. 1 sentence each. -->
```
