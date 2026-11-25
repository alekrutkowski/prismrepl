# Assessment of the supplied checktor report

Reviewed on 25 November 2026 against the reported source call sites, which
remain in prismrepl 0.6.4. This assesses the user's supplied report; it does
not claim a fresh checktor run or CRAN acceptance.

The 11 reported findings concern intentional console behavior or submission
metadata. None identifies the Windows binding-classification failure or the
inherited compiler flag. The Windows binding-classification failure is addressed separately in the
compatibility code and its regression tests. CRAN's own policy requires checking the final source
tarball and allows explanatory submission comments for findings believed to be
spurious; that is an assessment process, not an assurance of acceptance.
[CRAN submission policy](https://cran.r-project.org/web/packages/policies.html#Submission)

## Findings checked against the implementation

| Finding                         | Count | Source and assessment                                                                                                                                                                                                                                                        |
| ------------------------------- | ----- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `print()` / `cat()`             | 3     | `R/inspect.R` captures both flagged print calls with `utils::capture.output()` and returns preview text. `R/repl.R` writes the basic console's input prompt to stdout, reached through explicit interactive REPL use.                                                        |
| Unrestored option changes       | 1     | The flagged `R/repl.R` call is `setwd()` in `:cd`, not `options()`. Keeping the user-selected working directory is the command's purpose; restoring it immediately would break that feature.                                                                                 |
| Software installation           | 2     | `:install` and `:installgh` run only after the user submits those commands. They do not install dependencies during loading, examples or tests. Tests mock command evaluation. `remotes` is Suggested and its namespace is checked before use.                               |
| `library()` in package code     | 1     | `:attach PACKAGE ...` deliberately attaches packages chosen by the user. Replacing it with an Imports declaration or `pkg::fun()` would not implement the requested attachment.                                                                                              |
| Missing NEWS                    | 1     | NEWS is optional under R's package structure specification. It remains omitted according to the project's requested documentation style.                                                                                                                                     |
| Missing `cran-comments.md`      | 1     | The file exists in the development/release materials and is deliberately excluded by `.Rbuildignore`. CRAN resubmission explanations belong in the submission form's Optional comment field; embedding this file in the tarball is not a submission requirement.             |
| `system2()` calls               | 2     | `git_info()` quotes the normalized path, uses fixed read-only Git arguments and a two-second timeout. The clipboard helper uses fixed helper commands, passes clipboard data through stdin files, bounds execution/output, and removes its temporary files with `on.exit()`. |

The preview behavior follows R's documented
[`capture.output()` semantics](https://stat.ethz.ch/R-manual/R-devel/library/utils/html/capture.output.html).
The distinction between attaching a package and loading only its namespace is
specified in R's
[`library()` documentation](https://stat.ethz.ch/R-manual/R-devel/library/base/html/library.html).
The optional package files and build exclusions are described in
[Writing R Extensions](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Package-structure).
CRAN describes resubmission comments in its
[resubmission policy](https://cran.r-project.org/web/packages/policies.html).

## Scope and action

Keep the requested console, package-management and clipboard features. The
`repl()` entry point rejects noninteractive use, and `.onLoad()` only registers
completion-worker cleanup. Actual operating-system actions remain explicit
user operations. This source review does not treat a static diagnostic's
`policy` label as proof that CRAN has rejected an operation; the relevant
restrictions still apply, including secure network access, controlled external
processes, and user-directed filesystem/clipboard changes.
[CRAN source-package policy](https://cran.r-project.org/web/packages/policies.html#Source-packages)

The attachment also says the spelling check was skipped because no backend was
installed. That is a check environment limitation, not evidence of a spelling
error.

The compatibility fix and the explicit Ubuntu check-profile setup address
the two separately reported problems. Adding NEWS or placing submission
comments inside the tarball would not fix either issue.
