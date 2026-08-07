---
description: Run `codex review` and surface only the final review. Use whenever asked to "ask codex to review", "get a codex review", or otherwise invoke codex's code review subagent. Captures full transcript to a tempfile so failures stay debuggable.
---

# Running `codex review` cleanly

`codex review` writes its banner and every tool call it makes to **stderr**. The _final_ review prose is the only thing on **stdout**. Exploit this.

## Default invocation

```bash
log=$(mktemp -t codex-review.XXXXXXXX)
codex review --base <branch> 2>"$log"
echo "Full transcript: $log"
```

Substitute `<branch>` for the actual base (usually `main`). Swap in `--commit <sha>` or `--uncommitted` to scope the review differently.

If the review comes back empty, looks confused, or codex exits non-zero, read `$log` to see what actually happened.

## Structured findings (JSON)

When you want the machine-readable blob (findings array, confidence score) instead of prose:

```bash
log=$(mktemp -t codex-review.XXXXXXXX)
codex exec review --base <branch> --json 2>"$log" \
  | jq -r 'select(.type=="event_msg" and .payload.type=="task_complete") | .payload.last_agent_message'
echo "Full transcript: $log"
```

The emitted JSON has `findings[]`, `overall_correctness`, `overall_explanation`, `overall_confidence_score`.

## Notes

- Don't reach for `--output-last-message` — it's broken for the review subagent (writes an empty file even on success).
- Reviews typically take 1–4 minutes. Run with `run_in_background: true` if you have other work to pursue meanwhile.
