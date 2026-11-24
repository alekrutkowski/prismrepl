# Actual terminal renderer frames

These images rasterize the actual ANSI output and grapheme cell widths returned
by `prismrepl`'s fullscreen, palette, documentation, function-editor and object-explorer compositors. They are reproducible developer figures,
not screenshots of Windows Terminal or another operating system.

The example evaluates R code in a real session environment. It prints three rows
of an iris data frame, emits a message and a warning, and raises an error through
three nested user functions. A four-line expression remains in the editor.
Workspace information comes from the actual evaluation environment. The wide
and narrow dark frames use the same transcript, so they also demonstrate
reformatting after a resize.

| File                              | Theme    | Terminal columns x rows | Sidebar |
| --------------------------------- | -------- | ----------------------- | ------- |
| `fullscreen-midnight-wide.png`    | midnight | 132 x 40                | Visible |
| `fullscreen-midnight-narrow.png`  | midnight | 68 x 30                 | Hidden  |
| `fullscreen-paper-wide.png`       | paper    | 132 x 40                | Visible |
| `fullscreen-white-wide.png`       | white    | 132 x 40                | Visible |
| `fullscreen-black-wide.png`       | black    | 132 x 40                | Visible |
| `palette-white.png`               | white    | 132 x 36                | None    |
| `docs-white.png`                  | white    | 122 x 51                | None    |
| `editor-midnight.png`             | midnight | 122 x 24                | None    |
| `explorer-midnight.png`           | midnight | 132 x 25                | None    |
| `explorer-drilldown-midnight.png` | midnight | 132 x 28                | None    |
| `explorer-column-midnight.png`    | midnight | 132 x 25                | None    |

The palette figure uses the current command registry, real logical grouping,
paired rows and an `:attach` selection. Documentation comes from the installed
`base::mean` Rd page, converted through R's HTML renderer and Prism's semantic
terminal renderer. All 14 case-insensitive `mean` occurrences are highlighted;
the first occurrence has the active-match style. The function editor figure
starts with a real R closure and changes its quantile probabilities in the
editable source, showing the modified flag and actual insertion cursor.
Explorer
figures inspect an actual list and an evaluated `stats::lm()` fit, including
one Enter event to follow a child. The column figure opens `flowers$Petal.Width`
directly as a numeric preview, retaining the column breadcrumb without element
children. Modal panels hide the text insertion cursor;
the editor figures mark its real position with an outline.

Run from the repository root after installing the package and its dependencies:

```sh
Rscript tools/render-demo.R
python tools/ansi-to-png.py tools/figures/*.json
```

The R exporter loads the current `R/` sources into a private environment and uses
the installed package's native backend. It requires the developer dependency
`jsonlite`. The Python renderer requires Pillow and DejaVu Sans Mono; use
`--font-dir` to point to another installation of that font family.

The configuration requests truecolor explicitly, with automatic content width.
The exporter sets `NO_COLOR=1` and `cli.num_colors=1` in its own process to verify
that explicit native ANSI styling survives disabled automatic color detection.
Normal sessions still honor `NO_COLOR` when color is configured automatically.

The adjacent `.ansi` files contain original styled rows. The `.txt` files contain
the same rows with display controls removed. The `.json` files preserve the exact
graphemes, widths, control sequences, dimensions, and cursor position used to
generate each PNG. The renderer places those cells directly; it does not add
content, change the palette, or simulate unimplemented controls.

All of `tools/` is excluded from the CRAN source package by `.Rbuildignore`.

## Documentation displayed in the figure

The text displayed in `docs-white` belongs to the R Core Team's
[`base::mean` manual page](https://stat.ethz.ch/R-manual/R-devel/library/base/html/mean.html).
Its upstream [Rd source](https://svn.r-project.org/R/trunk/src/library/base/man/mean.Rd)
is part of R. That upstream documentation retains
[R's GPL-2 or GPL-3 distribution terms](https://www.r-project.org/Licenses/).
The Prism implementation and the figure-export tools use this project's MIT
license. The other figures use evaluated example data or original demonstration
source.

## Developer-tool acknowledgments

These tools are used only to reproduce the figures. They are not runtime
dependencies of the R console, and their code and font files are not bundled
with the package.

- [jsonlite](https://cran.r-project.org/package=jsonlite), by Jeroen Ooms and
  contributors, serializes the actual renderer cells to JSON. Its CRAN metadata
  specifies MIT plus its accompanying license file.
- [Pillow](https://pillow.readthedocs.io/en/stable/about.html), by Jeffrey 'Alex'
  Clark and contributors, rasterizes the cells. Pillow builds on PIL by Fredrik
  Lundh, Secret Labs AB, and contributors, and uses the
  [MIT-CMU license](https://github.com/python-pillow/Pillow/blob/main/LICENSE).
- [DejaVu Sans Mono](https://dejavu-fonts.github.io/), by the DejaVu Fonts
  contributors, supplies the glyphs. The project's
  [license page](https://dejavu-fonts.github.io/License.html) records the
  Bitstream Vera copyright and license, public-domain DejaVu changes, and
  applicable Arev glyph notices.
