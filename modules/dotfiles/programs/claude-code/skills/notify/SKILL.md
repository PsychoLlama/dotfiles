---
description: Send a desktop notification to get the user's attention. Use after a long-running task finishes, or when you need a response and the user is likely away from the terminal.
---

# Sending desktop notifications

- Run `${CLAUDE_SKILL_DIR}/notify <message>` to send a notification.
- Pass `--title <title>` or `--icon <icon>` before the message to override the defaults.
- Keep messages short — one line, no markdown.
- Don't notify for routine completions; reserve it for tasks the user explicitly waited on or builds/tests that took minutes.
