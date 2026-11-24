# Responsiveness measurements

prismrepl 0.5.1; 24 November 2026; R 4.6.2; Linux x86-64. These are the
current candidate's measurements. Raw values, repeat counts and writer counters
are in `performance.csv`; the complete script output is in `benchmark.log`.

## Actual terminal session

`pty-fullscreen.log` records 12 input edits at a 5 ms observation interval:
**median 5.12 ms; maximum 10.27 ms** from injected key to the corresponding
cursor update. This includes a real R process and POSIX pseudo-terminal.
The observer cannot reliably resolve sub-5-ms latency. It does not measure
a graphical terminal emulator's paint time.

## Instrumented renderer and editor

`tools/benchmark-render.R` ran against the installed, byte-compiled package,
with rainbow colors and automatic pairing enabled. The viewport was
132 columns by 40 rows. The compositor cases below retain 4,000 transcript
entries. Editor cases use the specified number of logical lines.

| Operation                                   | Fixture | Logical lines | Repetitions | Mean elapsed |
| ------------------------------------------- | ------- | ------------- | ----------- | ------------ |
| First composed frame                        | ascii   | Transcript    | 5           | 10.40 ms     |
| Cursor-only composition                     | ascii   | Transcript    | 100         | 0.13 ms      |
| Changed editor row                          | ascii   | Transcript    | 50          | 1.18 ms      |
| One-line transcript scroll                  | ascii   | Transcript    | 50          | 2.94 ms      |
| Editor cursor, geometry and highlighting    | ascii   | 1             | 100         | 0.11 ms      |
| Editor insertion, geometry and highlighting | ascii   | 1             | 20          | 3.25 ms      |
| Editor cursor, geometry and highlighting    | ascii   | 2000          | 100         | 0.21 ms      |
| Editor insertion, geometry and highlighting | ascii   | 2000          | 20          | 47.60 ms     |
| Editor cursor, geometry and highlighting    | unicode | 2000          | 100         | 0.25 ms      |
| Editor insertion, geometry and highlighting | unicode | 2000          | 20          | 73.30 ms     |

The writer is instrumented: compositor timings measure computation and
assembly, excluding OS transport, emulator painting and editor preparation.
Editor timings separately include replacement or cursor movement, geometry,
highlighting, clipping and hit maps. Each emitted compositor update reaches
one native writer invocation, with no R `cat()` calls or R-console flushes.
The OS may require multiple transport writes to complete one native call.
The byte counter covers R frame payloads, excluding native cursor-color controls
and synchronized-output wrappers. Cursor tint is sent only when it changes.

The Unicode fixture repeats `α <- f(list("é", x[[1]]), β)` 2,000 times.
The quoted character is `e` followed by U+0301. This exercises multibyte
identifiers, combining marks, quotes, nested calls and double-bracket indexing.
The lexer obtains Unicode offsets with stringi, decodes source code points
once, and extracts full token text only for the visible viewport and immediate
lookahead. A deterministic test guards viewport-bounded extraction; lexical
equivalence cases cover emoji, raw strings, escapes and malformed input.

Large-buffer edits still validate input and recompute token positions.
These observations describe this host and these fixtures, without asserting
a universal frame rate or a Windows/macOS latency guarantee. Fixed execution
order, cache warmth, scheduling and timer resolution affect the results;
the raw 40-entry and 4,000-entry rows are not an isolated scaling experiment.

## Reproduce

```sh
R CMD INSTALL .
PRISM_BENCH_CSV=tools/validation/performance.csv Rscript tools/benchmark-render.R
python3 tools/pty-fullscreen.py
```

Set `PRISM_R` to select the R executable for the PTY test. Set
`PRISM_BENCH_LIBRARY` to benchmark an isolated installed library.
