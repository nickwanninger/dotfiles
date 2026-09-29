---
name: deep-review
description: Review a git change set and walk the user through every group of changes one at a time, soliciting feedback after each group and collating all feedback into a final list of action items. Use when the user asks to deeply review changes — e.g. "deep-review the uncommitted changes", "deep-review the current branch", "deep-review this commit", "deep-review the changes to file X".
---

Go through every change in the requested range, break it into logical groups,
and interview the user about their thoughts of every change group with the
required context. Ask the user one group at a time. Don't leave anything out.

Do not modify files or make commits. This is a review-only workflow. The review is evidence-based:
ground every finding in the actual diff and surrounding code you read, not speculation.
