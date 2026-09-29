---
name: study
description: Run a reproducible, data-driven study of one system, feature, or behavior in a codebase (performance, bottlenecks, hardware costs, correctness). Measures, records raw data, and writes a report under ./studies/{name}.
disable-model-invocation: true
---

# Study

A study answers one precise question about a system with measurements. Another
person must be able to read the report, run one command, and get the same data
and plots. Evidence comes first: do not state a conclusion that the recorded
data does not support.

## 1. Find prior work, or resume

Look in `./studies/`. If a study on the same subject exists, read its README
(start with `## Status`), its Log, and its data. Then continue from the phase
and next step that Status records. Do not repeat steps that are complete, and
do not run the grill-me interview again unless the question changes. Start a
new study only if the user asks. Tell the user which option you pick.

## Resumability

A study can stop at any time (context limit, new session, user interrupt) and
another agent must continue it with no memory of this session. The files in
the study directory are the only state. So:

- Keep a `## Status` section at the top of README.md with: current phase
  (question, hypothesis, experiments, analysis, done), the active hypothesis,
  what is complete, what is in progress, and the next concrete step.
- Update Status and add a Log entry after each step, before you start the
  next one. Do not wait until the end.
- Write down decisions and user answers (from grill-me) in the README when
  you get them. Chat history is lost.
- Make each experiment a separate script that you can run alone. A failed or
  stopped experiment must not require the others to run again.

## 2. Narrow the question

Invoke the `grill-me` skill to interview the user until these are specific:

- The question (e.g. "Why is `vpgatherdq` slower than 8 scalar loads on this
  machine?", not "Why is gather slow?").
- The system under test: files, functions, binaries, hardware.
- What "good" and "bad" results look like, and the baseline for comparison.
- Constraints: time budget, machine access, permissions (e.g. `perf` needs
  `perf_event_paranoid` settings), tools that are available.

Do not measure until the question is narrow. A vague question gives data that
cannot answer anything.

## 3. Pre-register the hypothesis

Before you run experiments, write in the README:

- Hypothesis, and the predicted result if it is true.
- The result that will falsify it.
- The experiments and baselines you will use to separate it from other
  explanations.

Save this section, with the date, before data exists. The purpose is to stop
you from fitting a story to the data after you see it. If the data falsifies
the hypothesis, record that, then write a new hypothesis as a new section. Do
not edit the old one.

## 4. Build experiments

Use Python through `uv` scripts with inline dependencies (PEP 723 header), so
each script runs with `uv run script.py` and no setup:

```python
# /// script
# requires-python = ">=3.12"
# dependencies = ["numpy", "matplotlib"]
# ///
```

Native code (C, asm, Rust) for microbenchmarks is fine. Put it in the study
directory and build it from the scripts, so the build is part of the
reproduction.

Design experiments to isolate causes:

- Always measure a baseline next to the subject (e.g. scalar loads next to
  gather, the unmodified program next to the modified one).
- Change one variable at a time.
- Build alternative implementations to test a cause. For the gather example,
  write software emulations of gather (scalar loads, loads + shuffles) to find
  which part of the instruction is expensive.
- Use real sources: hardware counters (`perf stat`, `perf record`), profilers,
  instruction latency tools, real inputs. Do not use invented numbers.

## 5. Record data

Raw data is never overwritten. Each run writes a new file in `data/`, with a
timestamp in the name (e.g. `data/2026-09-29T14-03-11_gather_latency.csv`).
Keep failed and surprising runs too; they are evidence.

Plots and tables are derived. They read only from files in `data/`, never from
values in script memory or values typed by hand. So a plot can be regenerated
from the data at any time.

## 6. Workspace layout

```
studies/{name}/
├── README.md        # the report (see below)
├── run              # one entry point: rebuilds, runs all experiments, makes plots
├── scripts/         # uv scripts and native sources
├── data/            # raw data, append-only, timestamped
└── plots/           # generated from data/
```

`run` can be a shell script or a uv script. It must take the study from a clean
checkout to the full set of data and plots. Add flags if a full run is slow
(e.g. `./run --plots-only`, `./run gather_latency` to run one experiment).

## 7. Report (README.md)

Use this structure:

```markdown
# {Title}

## Status            <!-- phase, active hypothesis, done, in progress, next step -->
## Question          <!-- include decisions and constraints from grill-me -->
## Hypothesis        <!-- pre-registered, dated; later hypotheses as new subsections -->
## Method            <!-- experiments, baselines, what each one isolates -->
## Reproduce         <!-- exact commands -->
## Results           <!-- plots and tables, each linked to the data file it came from -->
## Conclusion        <!-- supported / falsified, with the evidence; open questions -->
## Log               <!-- dated entries: what you ran, what changed, dead ends -->
```

In Results, cite the data file for each claim. In Conclusion, state how
confident you are and what other explanations the data does not exclude.

## 8. Report to the user

Give the answer to the question, the key evidence (one or two numbers or a
plot), and the path to the study. Say clearly if the hypothesis was falsified
or if the data is not sufficient to decide.

## Other study types

The same workflow applies to:

- Bottlenecks in a system: profile under a real workload, find the hot path,
  test that a change to it moves the end-to-end metric.
- Inefficient parts of a program: profile, rank by cost, make one targeted
  experiment per candidate.
