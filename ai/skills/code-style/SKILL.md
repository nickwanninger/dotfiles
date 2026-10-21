---
name: code-style
description: Nick's C++ style and cleanup preferences. Use when writing, editing, reviewing, documenting, or cleaning up C++ code (.cpp, .h, .hpp), including requests to space out dense code.
---

# C++ code style and cleanup

Keep functions focused, steps visible, and changes small enough for Nick to
review in his editor.

## Layout

- Use the project's `.clang-format`. Do not change its configuration.
- Format only changed lines with `git clang-format <base>`. For unstaged
  changes, use `git clang-format --diff <base> | git apply`. Format new files
  completely. Ask before formatting an entire existing file.
- Give every `if`, `for`, and `while` body braces and its own lines, even
  for one statement. Do not put two statements on one line. Short accessors
  and functions with one expression can stay on one line.
- Put two blank lines between functions and three before a section header.
- Put one blank line between logical steps: validation, setup, main work,
  and result. Treat a guard clause block as one step.
- Separate groups of class members with a blank line or a short label comment.
- In long `.cpp` files, group functions in execution order: construction,
  steps per frame, then teardown. Keep helpers in the section that uses them.
  Give each group a header in this format:

```cpp
  //=========================
  // Barriers and recording
  //=========================
```

## Comments

- Explain reasons that the code cannot show, such as ordering, lifetimes,
  and GPU synchronization. Do not narrate individual statements.
- Put `//` comments above the code. Use trailing comments only for short notes.
- Give each public declaration in a header a comment of one to four lines:
  purpose, when to call it, and unusual return values or exceptions.
- Describe each class's purpose and how its parts fit together. List any
  required call order.
- Use one term for each concept. Write short sentences in simple present
  tense and active voice. Omit filler and marketing words.
- Use `// TODO:` for known work. Phrase open design decisions as questions.
- State intended behavior. If code contradicts a comment, report the bug;
  do not document the bug as intended behavior.

## Code conventions

- Match the file or class for member style (`m_name` or `this->name`) and
  empty parameter lists (`f()` or `f(void)`).
- Use `PascalCase` for types, `camelCase` for functions and locals, `getX()`
  for getters, and `isX()` or `hasX()` for predicates.
- Find and use nearby project aliases and helpers: smart pointers and their
  factories, integer aliases, logging functions, and profiling macros.
  Prefer these to raw standard-library forms.
- Report errors with exceptions that name the object, for example:
  `throw std::runtime_error("Resource allocation returned null: " + name);`.
- Name header parameters when the type alone does not explain their purpose.
- Search for every use before removing unused parameters, members, or functions.
  Remove those with no uses.
- Put validation and guard clauses first. Return early.
- Extract a job into a named helper when it has its own policy or reason to
  change. Keep short steps inline with a comment.
- Extract a helper when three or more functions repeat the same checks.
  Preserve error messages that tests depend on.
- Do not add an abstraction just to remove a few repeated lines.

## Cleanup

1. Read the whole file and its dependencies before proposing changes. Base
   comments on the code, not assumptions from names.
2. Plan passes for section order, spacing, helper extraction, and removals.
   Separate layout changes from moves or extractions. Ask before the latter.
   Do not move code to another file; propose the move for Nick to decide.
3. Change one section at a time. After each section, build, run tests, and
   verify formatting with clang-format. Report changes beyond spacing,
   including helpers, error messages, and removals.
4. Preserve behavior. Report bugs with file and line; fix them only with
   Nick's agreement.
5. Investigate build failures in unchanged code before further edits. Nick
   can edit the same files at the same time.

For header documentation, work through one group of declarations at a time.
Show proposed comments, identify incorrect comments, and ask open questions.
Apply the text only after Nick agrees.
