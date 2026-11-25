# Sending desktop notifications

- Run `{{ SKILL_DIRECTORY }}/notify <message>` to send a notification.
- Pass `--title <title>` or `--icon <icon>` before the message to override the defaults.
- Keep messages short — one line, no markdown.
- Don't notify for routine completions; reserve it for tasks the user explicitly waited on or builds/tests that took minutes.
