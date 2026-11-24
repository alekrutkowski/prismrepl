.help_cache <- new.env(parent = emptyenv())
.help_page_cache <- new.env(parent = emptyenv())
.help_html_cache <- new.env(parent = emptyenv())

#' Render installed R documentation as Unicode text
#'
#' Converts installed Rd documentation with R's own renderer, requests UTF-8,
#' and disables overstruck headings so backspaces cannot damage the screen.
#' Ordinary Rd links remain readable text. Package databases and rendered pages
#' are cached in memory, with limits of eight packages and 32 pages. Formatting
#' option changes invalidate page entries, and dynamic Rd expressions bypass
#' the rendered-page cache. A package index is returned when
#' `topic = NULL`. Results are suitable for the REPL's searchable pager.
#'
#' @param topic One help topic, including an operator such as `"["`, or a
#'   qualified topic such as `"stats::lm"`. Use `NULL` for a package index.
#' @param package Optional package name. If omitted, topic lookup follows R's
#'   help search. With no topic and no package, prismrepl's index is used.
#' @param width Target page width in terminal columns.
#' @return A character vector of terminal-safe UTF-8 lines. If no help is found,
#'   returns a short explanatory message.
#' @export
#' @examples
#' head(help_text("mean", package = "base"))
#' head(help_text(package = "stats"))
help_text <- function(topic = NULL, package = NULL, width = 80L) {
  stopifnot("topic must be NULL or one nonempty string" = is.null(topic) ||
              (is.character(topic) && length(topic) == 1L && !is.na(topic) && nzchar(topic)),
            "package must be NULL or one nonempty string" = is.null(package) ||
              (is.character(package) && length(package) == 1L && !is.na(package) && nzchar(package)),
            "width must be a finite scalar at least 20" = is.numeric(width) &&
              length(width) == 1L && is.finite(width) && width >= 20)
  width <- as.integer(min(width, 10000L))
  if (!is.null(topic) && grepl("::", topic, fixed = TRUE)) {
    parts <- strsplit(topic, "::", fixed = TRUE)[[1L]]
    stopifnot("qualified topic must have one package and one topic" =
                length(parts) == 2L && all(nzchar(parts)),
              "qualified topic must agree with package" =
                is.null(package) || identical(package, parts[[1L]]))
    package <- parts[[1L]]
    topic <- parts[[2L]]
  }
  if (is.null(topic)) {
    if (is.null(package)) package <- "prismrepl"
    out <- .package_help_text(package, width)
  } else {
    paths <- suppressWarnings(tryCatch(utils::help((topic), package = (package),
      help_type = "text", try.all.packages = FALSE, verbose = FALSE),
      error = function(e) character()))
    if (is.null(package) && length(paths)) {
      package <- basename(dirname(dirname(as.character(paths)[[1L]])))
    }
    if (is.null(package) && length(find.package(topic, quiet = TRUE))) {
      out <- .package_help_text(topic, width)
    } else if (is.null(package)) {
      out <- paste0("No documentation found for '", topic,
        "'. Try a qualified topic such as 'stats::lm'.")
    } else {
      db <- tryCatch(.help_rd_db(package), error = function(e) list())
      index <- attr(db, "prismrepl_alias_index")
      matches <- if (is.null(index)) integer() else unname(index[topic])
      matches <- matches[!is.na(matches)]
      if (!length(matches)) {
        out <- paste0("No documentation found for '", topic,
                      "' in package '", package, "'.")
      } else {
        out <- .help_cached_render(package, matches[[1L]], db[[matches[[1L]]]], width)
      }
    }
  }
  out <- .safe_text(enc2utf8(out))
  stopifnot("documentation must be nonmissing text" = is.character(out) && !anyNA(out),
            "documentation must not contain backspaces" = !any(grepl("\b", out, fixed = TRUE)))
  out
}

#' Read and cache installed documentation
#' @param package One installed package name.
#' @return A named list of Rd objects.
#' @keywords internal
#' @noRd
.help_rd_db <- function(package) {
  stopifnot("package must be a nonempty string" = is.character(package) &&
              length(package) == 1L && !is.na(package) && nzchar(package))
  path <- find.package(package)
  key <- paste0(path, "@", as.character(utils::packageVersion(package)))
  if (!exists(key, .help_cache, inherits = FALSE)) {
    if (length(ls(.help_cache, all.names = TRUE)) >= 8L)
      rm(list = ls(.help_cache, all.names = TRUE)[[1L]], envir = .help_cache)
    db <- tools::Rd_db(package, lib.loc = dirname(path))
    aliases <- lapply(db, .rd_aliases)
    labels <- unlist(aliases, use.names = FALSE)
    ids <- rep.int(seq_along(db), lengths(aliases))
    keep <- !duplicated(labels)
    attr(db, "prismrepl_alias_index") <- stats::setNames(ids[keep], labels[keep])
    assign(key, db, .help_cache)
  }
  out <- get(key, .help_cache, inherits = FALSE)
  stopifnot("Rd database must be a list" = is.list(out))
  out
}

#' Read documentation aliases without evaluation
#' @param rd An Rd object.
#' @return A character vector of aliases.
#' @keywords internal
#' @noRd
.rd_aliases <- function(rd) {
  stopifnot("rd must be a list" = is.list(rd))
  entries <- Filter(function(x) identical(attr(x, "Rd_tag"), "\\alias"), rd)
  out <- vapply(entries, function(x) paste0(unlist(x), collapse = ""), character(1))
  stopifnot("aliases must be character" = is.character(out))
  out
}

#' Produce an installed package's help index
#' @param package One package name.
#' @param width Target text width.
#' @return A character vector of page lines.
#' @keywords internal
#' @noRd
.package_help_text <- function(package, width) {
  stopifnot("package must be a string" = is.character(package) &&
              length(package) == 1L && !is.na(package),
            "width must be at least 20" = is.numeric(width) && length(width) == 1L && width >= 20)
  if (!length(find.package(package, quiet = TRUE)))
    return(paste0("Package '", package, "' is not installed."))
  desc <- utils::packageDescription(package)
  field <- function(name) {
    value <- desc[[name]]
    if (is.null(value)) "" else as.character(value)
  }
  db <- tryCatch(.help_rd_db(package), error = function(e) list())
  topics <- vapply(db, function(rd) {
    aliases <- .rd_aliases(rd)
    if (!length(aliases)) return("")
    title <- Filter(function(x) identical(attr(x, "Rd_tag"), "\\title"), rd)
    title <- if (!length(title)) "" else
      trimws(gsub("\\s+", " ", paste0(unlist(title[[1L]]), collapse = "")))
    paste0(aliases[[1L]], "  ", title)
  }, character(1), USE.NAMES = FALSE)
  topics <- sort(topics[nzchar(topics)])
  out <- c(paste0(package, " ", field("Version")), field("Title"), "",
           strwrap(field("Description"), width = width), "", "Topics", "",
           topics, "", paste0("Open a topic with :doc ", package, "::topic"))
  stopifnot("package help must be text" = is.character(out) && !anyNA(out))
  out
}


#' Convert an Rd object without overstruck terminal output
#' @param rd An Rd object.
#' @param package The package label.
#' @param width Page width.
#' @return Safe UTF-8 lines.
#' @keywords internal
#' @noRd
.help_render_rd <- function(rd, package, width) {
  stopifnot("rd must be a list" = is.list(rd),
            "package must be a scalar string" = is.character(package) &&
              length(package) == 1L && !is.na(package),
            "width must be a positive scalar" = is.numeric(width) &&
              length(width) == 1L && is.finite(width) && width > 0)
  out <- tryCatch(utils::capture.output(tools::Rd2txt(rd,
    out = "", package = package, outputEncoding = "UTF-8",
    options = list(width = width, underline_titles = FALSE, showURLs = TRUE))),
    error = function(e) paste0("Documentation rendering failed: ", conditionMessage(e)))
  out <- .safe_text(out)
  stopifnot("rendered help must be text without missing values" =
              is.character(out) && !anyNA(out))
  out
}


#' Reuse a rendered page when its formatting settings are unchanged
#' @param package An installed package name.
#' @param index Document index within the package database.
#' @param rd The selected Rd object.
#' @param width Target page width.
#' @return Safe UTF-8 page lines.
#' @keywords internal
#' @noRd
.help_cached_render <- function(package, index, rd, width) {
  stopifnot("package must be one string" = is.character(package) && length(package) == 1L,
            "index must be a positive scalar" = is.numeric(index) && length(index) == 1L && index > 0,
            "rd must be a list" = is.list(rd),
            "width must be a positive scalar" = is.numeric(width) && length(width) == 1L && width > 0)
  if (.help_dynamic_rd(rd)) return(.help_render_rd(rd, package, width))
  key <- paste(find.package(package), as.character(utils::packageVersion(package)),
               index, width, sep = "|")
  settings <- list(format = tools::Rd2txt_options(),
                   fancy = getOption("useFancyQuotes"), locale = Sys.getlocale("LC_CTYPE"))
  if (exists(key, .help_page_cache, inherits = FALSE)) {
    cached <- get(key, .help_page_cache, inherits = FALSE)
    if (identical(cached$settings, settings)) return(cached$text)
  }
  out <- .help_render_rd(rd, package, width)
  if (!any(startsWith(out, "Documentation rendering failed:"))) {
    keys <- ls(.help_page_cache, all.names = TRUE)
    if (!key %in% keys && length(keys) >= 32L)
      rm(list = keys[[1L]], envir = .help_page_cache)
    assign(key, list(settings = settings, text = out), .help_page_cache)
  }
  stopifnot("cached help must be nonmissing text" = is.character(out) && !anyNA(out))
  out
}

#' Detect dynamic Rd code that must not be frozen in a rendered-page cache
#' @param node An Rd tree node.
#' @return A logical scalar.
#' @keywords internal
#' @noRd
.help_dynamic_rd <- function(node) {
  stopifnot("Rd nodes must be lists or text" = is.list(node) || is.character(node))
  out <- is.list(node) && (identical(attr(node, "Rd_tag"), "\\Sexpr") ||
    any(vapply(node, .help_dynamic_rd, logical(1))))
  stopifnot("dynamic marker must be scalar logical" = is.logical(out) && length(out) == 1L)
  out
}

#' Render Installed Documentation with Semantic Terminal Styles
#' @param topic Topic, including package-qualified topics, or NULL.
#' @param package Optional package.
#' @param width Available content cells.
#' @param config Console configuration.
#' @return Styled UTF-8 lines; ordinary help_text remains a plain-text API.
#' @noRd
.help_rich_text <- function(topic, package, width, config) {
  stopifnot("topic must be NULL or text" = is.null(topic) || .is_text(topic),
            "package must be NULL or text" = is.null(package) || .is_text(package),
            "width must be at least twenty" = .is_count(width, 20L),
            "config must be a configuration" = inherits(config, "prismrepl_config"))
  if (!is.null(topic) && grepl("::", topic, fixed = TRUE)) {
    parts <- strsplit(topic, "::", fixed = TRUE)[[1L]]
    stopifnot("qualified topic must identify one package and topic" = length(parts) == 2L && all(nzchar(parts)),
              "qualified package must agree" = is.null(package) || identical(package, parts[[1L]]))
    package <- parts[[1L]]
    topic <- parts[[2L]]
  }
  if (!is.null(topic) && is.null(package)) {
    paths <- suppressWarnings(tryCatch(utils::help((topic), help_type = "text",
      try.all.packages = FALSE, verbose = FALSE), error = function(e) character()))
    if (length(paths)) package <- basename(dirname(dirname(as.character(paths)[[1L]])))
    else if (length(find.package(topic, quiet = TRUE))) { package <- topic; topic <- NULL }
  }
  if (is.null(topic)) {
    if (is.null(package)) package <- "prismrepl"
    out <- .package_help_text(package, width)
    heading <- seq_along(out) %in% 1:2 | out == "Topics"
    out[heading] <- style_text(out[heading], "heading", config$theme, config$color)
  } else if (is.null(package)) out <- help_text(topic, package, width) else {
    db <- tryCatch(.help_rd_db(package), error = function(e) list())
    index <- unname(attr(db, "prismrepl_alias_index")[topic])
    if (!length(index) || is.na(index[[1L]])) out <- help_text(topic, package, width) else {
      rd <- db[[index[[1L]]]]
      key <- paste(find.package(package), as.character(utils::packageVersion(package)), index[[1L]], sep = "|")
      dynamic <- .help_dynamic_rd(rd)
      html <- if (!dynamic) get0(key, .help_html_cache, inherits = FALSE) else NULL
      if (is.null(html)) {
        html <- tryCatch(utils::capture.output(tools::Rd2HTML(rd, out = "", package = package,
          outputEncoding = "UTF-8", standalone = FALSE)), error = function(e) NULL)
        if (!is.null(html) && !dynamic) {
          keys <- ls(.help_html_cache, all.names = TRUE)
          if (length(keys) >= 32L) rm(list = keys[[1L]], envir = .help_html_cache)
          assign(key, html, .help_html_cache)
        }
      }
      out <- if (is.null(html)) help_text(topic, package, width) else .help_html_lines(html, width, config)
    }
  }
  out <- vapply(enc2utf8(out), .screen_safe_ansi, character(1L), USE.NAMES = FALSE)
  stopifnot("rich documentation must be nonmissing UTF-8 text" = is.character(out) && !anyNA(out),
            "rich documentation must not contain terminal overstrikes" = !any(grepl("\b", out, fixed = TRUE)))
  out
}

#' Decode Character References Emitted by R's HTML Documentation Renderer
#' @param text One UTF-8 string.
#' @return Decoded text; unsupported references remain readable literals.
#' @noRd
.help_html_entities <- function(text) {
  stopifnot("text must be a scalar string" = is.character(text) && length(text) == 1L && !is.na(text))
  known <- c(amp = "&", lt = "<", gt = ">", quot = '"', apos = "'", nbsp = " ",
    ensp = " ", emsp = " ", thinsp = " ", ndash = "\u2013", mdash = "\u2013",
    minus = "\u2212", times = "\u00d7", divide = "\u00f7", plusmn = "\u00b1",
    lsquo = "\u2018", rsquo = "\u2019", ldquo = "\u201c", rdquo = "\u201d",
    hellip = "\u2026", bull = "\u2022", middot = "\u00b7", copy = "\u00a9",
    reg = "\u00ae", deg = "\u00b0", micro = "\u00b5", prime = "\u2032", Prime = "\u2033",
    le = "\u2264", ge = "\u2265", ne = "\u2260", equiv = "\u2261", asymp = "\u2248",
    infin = "\u221e", radic = "\u221a", sum = "\u2211", prod = "\u220f", int = "\u222b",
    isin = "\u2208", notin = "\u2209", empty = "\u2205", forall = "\u2200", exist = "\u2203",
    rarr = "\u2192", larr = "\u2190", harr = "\u2194", rArr = "\u21d2", hArr = "\u21d4",
    alpha = "\u03b1", beta = "\u03b2", gamma = "\u03b3", delta = "\u03b4", epsilon = "\u03b5",
    zeta = "\u03b6", eta = "\u03b7", theta = "\u03b8", iota = "\u03b9", kappa = "\u03ba",
    lambda = "\u03bb", mu = "\u03bc", nu = "\u03bd", xi = "\u03be", omicron = "\u03bf",
    pi = "\u03c0", rho = "\u03c1", sigma = "\u03c3", tau = "\u03c4", upsilon = "\u03c5",
    phi = "\u03c6", chi = "\u03c7", psi = "\u03c8", omega = "\u03c9", Gamma = "\u0393",
    Delta = "\u0394", Theta = "\u0398", Lambda = "\u039b", Xi = "\u039e", Pi = "\u03a0",
    Sigma = "\u03a3", Upsilon = "\u03a5", Phi = "\u03a6", Psi = "\u03a8", Omega = "\u03a9")
  matches <- gregexpr("&(?:#[xX][0-9A-Fa-f]+|#[0-9]+|[A-Za-z][A-Za-z0-9]+);", text, perl = TRUE)
  tokens <- regmatches(text, matches)[[1L]]
  if (!length(tokens)) return(text)
  replacements <- vapply(tokens, function(token) {
    name <- substr(token, 2L, nchar(token) - 1L)
    if (name %in% names(known)) return(unname(known[[name]]))
    if (!startsWith(name, "#")) return(token)
    number <- suppressWarnings(if (grepl("^#[xX]", name)) strtoi(substring(name, 3L), 16L)
      else as.double(substring(name, 2L)))
    if (!is.finite(number) || number < 1 || number > 1114111 || (number >= 55296 && number <= 57343))
      return("\ufffd")
    intToUtf8(as.integer(number))
  }, character(1L), USE.NAMES = FALSE)
  regmatches(text, matches) <- list(replacements)
  stopifnot("entity decoding must return one string" = length(text) == 1L && !is.na(text))
  text
}

#' Apply Documentation Emphasis Without Executable Terminal Controls
#' @param text Plain documentation.
#' @param tags Active HTML tag names.
#' @param config Configuration.
#' @return Safe inline styles.
#' @noRd
.help_inline_style <- function(text, tags, config) {
  stopifnot("text must be one string" = is.character(text) && length(text) == 1L && !is.na(text),
            "tags must be text" = is.character(tags),
            "config must be a configuration" = inherits(config, "prismrepl_config"))
  text <- .safe_text(text)
  out <- if ("code" %in% tags) highlight_r(text, config$theme, config$color, rainbow = config$rainbow) else
    if ("a" %in% tags) style_text(text, "link", config$theme, config$color) else text
  flags <- c(if (any(tags %in% c("b", "strong", "th", "dt"))) "1",
             if (any(tags %in% c("i", "em", "var", "cite"))) "3")
  if (config$color && config$theme$color_depth > 0L && length(flags)) {
    start <- paste0("\033[", paste(flags, collapse = ";"), "m")
    out <- paste0(start, gsub("\033[0m", paste0("\033[0m", start), out, fixed = TRUE), "\033[0m")
  }
  stopifnot("inline styling must preserve visible characters" =
    identical(gsub(.screen_ansi_pattern(), "", out, perl = TRUE), text))
  out
}

#' Wrap Styled Prose at Words and Unicode Grapheme Boundaries
#' @param text One safe styled paragraph.
#' @param width Target cells.
#' @param indent Indentation of continuation lines.
#' @return Styled physical lines.
#' @noRd
.help_wrap_prose <- function(text, width, indent = 0L) {
  stopifnot("text must be a scalar string" = is.character(text) && length(text) == 1L && !is.na(text),
            "width must be positive" = .is_count(width, 1L),
            "indent must fit the width" = .is_count(indent) && indent < width)
  if (.screen_width(text) <= width) return(text)
  parts <- .screen_tokens(text)
  chars <- parts$tokens[parts$printable]
  widths <- parts$widths[parts$printable]
  out <- character()
  start <- 1L
  while (start <= length(chars)) {
    prefix <- if (length(out)) strrep(" ", indent) else ""
    limit <- width - nchar(prefix)
    candidate <- which(cumsum(widths[seq.int(start, length(chars))]) <= limit)
    end <- if (length(candidate)) start + utils::tail(candidate, 1L) - 1L else start
    if (end < length(chars)) {
      gaps <- which(chars[seq.int(start, end)] %in% c(" ", "\t")) + start - 1L
      if (length(gaps) && utils::tail(gaps, 1L) > start) end <- utils::tail(gaps, 1L) - 1L
    }
    out <- c(out, paste0(prefix, .screen_token_slice(parts, start, end)))
    start <- end + 1L
    while (start <= length(chars) && chars[[start]] %in% c(" ", "\t")) start <- start + 1L
  }
  stopifnot("prose wrapping must produce nonmissing lines" = is.character(out) && length(out) > 0L && !anyNA(out))
  out
}

#' Convert R-Generated Documentation HTML to Rich Terminal Lines
#'
#' Only generated semantic tags are interpreted. Scripts, styles, browser
#' controls and terminal controls have no executable path in this renderer.
#' Unknown markup is transparent and unknown entities stay readable.
#' @param html HTML produced by tools::Rd2HTML with standalone FALSE.
#' @param width Prose width; preformatted code retains horizontal scrolling.
#' @param config Configuration.
#' @return Safe ANSI-styled UTF-8 lines.
#' @noRd
.help_html_lines <- function(html, width, config) {
  stopifnot("HTML must be text" = is.character(html) && !anyNA(html),
            "width must be at least twenty" = .is_count(width, 20L),
            "config must be a configuration" = inherits(config, "prismrepl_config"))
  html <- paste(html, collapse = "\n")
  html <- gsub("(?s)<!--.*?-->", "", html, perl = TRUE)
  tokens <- regmatches(html, gregexpr("<[^>]*>|[^<]+|<", html, perl = TRUE))[[1L]]
  tags <- character()
  runs <- list()
  output <- character()
  lists <- list()
  kind <- "paragraph"
  indent <- 0L
  table_column <- 0L
  add <- function(text, forced_tags = tags) {
    if (!nzchar(text)) return(invisible(NULL))
    text <- .help_html_entities(text)
    if (!"pre" %in% tags) {
      text <- gsub("[ \t\r\n]+", " ", text, perl = TRUE)
      if (!length(runs) || endsWith(runs[[length(runs)]]$text, " ")) text <- sub("^ +", "", text)
    }
    if (nzchar(text)) runs[[length(runs) + 1L]] <<- list(text = text, tags = forced_tags)
    invisible(NULL)
  }
  flush <- function(blank = FALSE) {
    if (length(runs)) {
      runs[[length(runs)]]$text <<- sub("[ \r\n]+$", "", runs[[length(runs)]]$text)
      plain <- paste0(vapply(runs, `[[`, character(1L), "text"), collapse = "")
      if (nzchar(plain)) {
        if (kind == "code") {
          code <- .safe_text(plain)
          rendered <- .screen_split_lines(highlight_r(code, config$theme, config$color, rainbow = config$rainbow))
        } else if (kind == "pre") rendered <- .screen_split_lines(.safe_text(plain)) else {
          styled <- paste0(vapply(runs, function(run) .help_inline_style(run$text, run$tags, config), character(1L)), collapse = "")
          if (kind == "heading") styled <- style_text(styled, "heading", config$theme, config$color)
          rendered <- .help_wrap_prose(styled, width, min(indent, width - 1L))
        }
        output <<- c(output, rendered)
      }
      runs <<- list()
    }
    if (blank && length(output) && nzchar(utils::tail(output, 1L))) output <<- c(output, "")
    invisible(NULL)
  }
  invisible(lapply(tokens, function(token) {
    if (!startsWith(token, "<") || identical(token, "<")) {
      if (!any(tags %in% c("script", "style"))) add(token)
      return(NULL)
    }
    closing <- grepl("^</", token)
    tag <- tolower(sub("^</?([A-Za-z][A-Za-z0-9]*).*", "\\1", token))
    if (!grepl("^[a-z][a-z0-9]*$", tag)) return(NULL)
    if (closing) {
      if (tag %in% c("h1", "h2", "h3", "h4", "h5", "h6", "pre")) {
        flush(TRUE); kind <<- "paragraph"
      } else if (tag %in% c("p", "div", "blockquote")) {
        flush(!any(tags %in% c("td", "th")))
      } else if (tag == "li") { flush(); indent <<- 0L }
      else if (tag %in% c("ul", "ol")) {
        flush(TRUE)
        if (length(lists)) lists <<- utils::head(lists, -1L)
      } else if (tag %in% c("tr", "dd", "table", "dl")) { flush(TRUE); table_column <<- 0L }
      else if (tag == "dt") add(": ", forced_tags = character())
      else if (tag %in% c("td", "th") && length(runs)) add(if (table_column == 1L) ": " else "  ", forced_tags = character())
      where <- which(tags == tag)
      if (length(where)) tags <<- utils::head(tags, utils::tail(where, 1L) - 1L)
      return(NULL)
    }
    if (any(tags %in% c("script", "style"))) { tags <<- c(tags, tag); return(NULL) }
    if (tag == "br") { flush(); return(NULL) }
    if (tag == "hr") { flush(TRUE); return(NULL) }
    if (tag == "img") {
      alt <- regmatches(token, regexec("alt=['\"]([^'\"]*)['\"]", token, perl = TRUE))[[1L]]
      if (length(alt) == 2L) add(paste0("[", alt[[2L]], "]"))
      return(NULL)
    }
    if (tag %in% c("h1", "h2", "h3", "h4", "h5", "h6")) { flush(TRUE); kind <<- "heading" }
    else if (tag == "pre") { flush(TRUE); kind <<- "pre" }
    else if (tag == "code" && "pre" %in% tags && grepl("language-R", token, fixed = TRUE)) kind <<- "code"
    else if (tag %in% c("p", "div", "blockquote") && !any(tags %in% c("td", "th", "dd", "li"))) flush(TRUE)
    else if (tag %in% c("ul", "ol")) { flush(TRUE); lists[[length(lists) + 1L]] <<- list(tag = tag, count = 0L) }
    else if (tag == "li") {
      flush()
      if (length(lists)) {
        level <- length(lists)
        lists[[level]]$count <<- lists[[level]]$count + 1L
        add(if (lists[[level]]$tag == "ol") paste0(lists[[level]]$count, ". ") else
          if (isTRUE(config$unicode)) "\u2022 " else "* ", forced_tags = character())
        indent <<- min(width - 1L, 2L)
      }
    } else if (tag %in% c("tr", "dt")) { flush(TRUE); table_column <<- 0L }
    else if (tag %in% c("td", "th")) table_column <<- table_column + 1L
    if (!grepl("/>", token, fixed = TRUE) && !tag %in% c("meta", "link", "input", "wbr")) tags <<- c(tags, tag)
    NULL
  }))
  flush()
  while (length(output) && !nzchar(utils::tail(output, 1L))) output <- utils::head(output, -1L)
  if (!length(output)) output <- "(Documentation page is empty.)"
  output <- vapply(output, .screen_safe_ansi, character(1L), USE.NAMES = FALSE)
  stopifnot("HTML terminal rendering must be safe nonmissing text" = is.character(output) &&
              length(output) > 0L && !anyNA(output) && !any(grepl("\n", output, fixed = TRUE)))
  output
}
