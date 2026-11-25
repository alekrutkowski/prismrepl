#' Escape Object Labels for a Single Terminal Row
#' @param text Character labels.
#' @return Labels without executable controls or embedded newlines.
#' @noRd
.explorer_text <- function(text) {
  stopifnot("labels must be nonmissing text" = is.character(text) && !anyNA(text))
  out <- stringi::stri_replace_all_fixed(.safe_text(text), "\n", "\\n")
  stopifnot("labels must remain single-line text" = is.character(out) &&
              !any(grepl("\n", out, fixed = TRUE)))
  out
}

#' Obtain the Ordinary Underlying Value of an S3 Object
#' @param x An object.
#' @return An unclassed value, or the original S4/reference value.
#' @noRd
.explorer_raw <- function(x) {
  out <- if (is.object(x) && !isS4(x) && !is.environment(x)) unclass(x) else x
  stopifnot("unclassing must preserve storage type" = typeof(out) == typeof(x))
  out
}

#' Recognize an Atomic Preview Leaf
#' @param x An inspected value.
#' @return A logical scalar, without invoking custom methods.
#' @noRd
.explorer_leaf <- function(x) {
  out <- is.null(x) || (!isS4(x) && is.atomic(x))
  stopifnot("leaf status must be logical" = .is_flag(out))
  out
}

#' Preserve Only Known Safe Atomic Display Classes
#' @param x An atomic object.
#' @return A factor/Date attribute template, or NULL for ordinary underlying values.
#' @noRd
.explorer_template <- function(x) {
  cls <- attr(x, "class", exact = TRUE)
  levels <- attr(x, "levels", exact = TRUE)
  out <- if (typeof(x) %in% c("integer", "double") && "factor" %in% cls && is.character(levels))
    list(class = "factor", levels = levels) else if (typeof(x) %in% c("integer", "double") &&
      identical(cls, "Date")) list(class = "Date") else NULL
  stopifnot("display template must be absent or a known atomic class" = is.null(out) ||
              out$class %in% c("factor", "Date"))
  out
}

#' Restore a Known Atomic Display Template to a Selected Value
#' @param value An explicitly selected ordinary value.
#' @param template Safe attributes or NULL.
#' @return The selected value, with only supported display attributes restored.
#' @noRd
.explorer_restore <- function(value, template) {
  stopifnot("template must be NULL or a list" = is.null(template) || is.list(template))
  if (!is.null(template) && !is.null(value)) attributes(value) <- template
  value
}

#' Read Structural Metadata Without Dispatching User Methods
#' @param x An object.
#' @param sizes Calculate the approximate size once for an opened node.
#' @return A named metadata list.
#' @noRd
.explorer_meta <- function(x, sizes = FALSE) {
  stopifnot("sizes must be logical" = .is_flag(sizes))
  type <- typeof(x)
  cls <- attr(x, "class", exact = TRUE)
  if (is.null(cls)) cls <- switch(type, double = "numeric", closure = "function",
    builtin = "function", special = "function", type)
  dims <- attr(x, "dim", exact = TRUE)
  n <- if (is.environment(x) || isS4(x)) NA_real_ else as.double(length(.explorer_raw(x)))
  if (is.list(x) && !isS4(x) && inherits(x, "data.frame"))
    dims <- c(length(attr(x, "row.names", exact = TRUE)), n)
  bytes <- if (sizes) tryCatch(as.double(utils::object.size(x)), error = function(e) NA_real_) else NA_real_
  shape <- if (length(dims)) paste(dims, collapse = " x ") else if (!is.na(n)) paste0("length ", n) else ""
  out <- list(type = type, class = .explorer_text(paste(cls, collapse = "/")), length = n,
    dimensions = dims, shape = shape, attributes = names(attributes(x)), bytes = bytes,
    size = if (is.na(bytes)) "" else paste0("~", format(structure(bytes, class = "object_size"), units = "auto")))
  stopifnot("metadata must contain scalar type and class" = .is_text(out$type) &&
              is.character(out$class) && length(out$class) == 1L,
            "approximate size must be unavailable or nonnegative" = is.na(out$bytes) || out$bytes >= 0)
  out
}

#' Summarize a Value Without Calling Print, Format, or Subset Methods
#' @param x An object.
#' @param items Maximum atomic elements to include.
#' @return A short, terminal-safe string.
#' @noRd
.explorer_brief <- function(x, items = 3L) {
  stopifnot("items must be a positive integer" = .is_count(items, 1L))
  type <- typeof(x)
  out <- if (is.null(x)) "NULL" else if (isS4(x)) "<S4 object>" else if (is.environment(x))
    "<environment; open bindings safely>" else if (is.function(x)) "<function; inspect body and formals>" else
    if (is.atomic(x)) {
      raw <- .explorer_raw(x)
      n <- length(raw)
      value <- raw[seq_len(min(items, n))]
      template <- .explorer_template(x)
      if (!is.null(template) && identical(template$class, "factor")) {
        labels <- rep(NA_character_, length(value))
        valid <- !is.na(value) & is.finite(value) & value == trunc(value) & value >= 1L & value <= length(template$levels)
        labels[valid] <- template$levels[value[valid]]
        value <- encodeString(substr(labels, 1L, 512L), quote = '"')
      } else if (!is.null(template) && identical(template$class, "Date")) {
        value <- tryCatch(base::format.Date(structure(value, class = "Date"), format = "%Y-%m-%d"),
          error = function(e) formatC(value, digits = 6L, format = "g", width = 1L))
      } else if (is.character(value)) value <- encodeString(substr(value, 1L, 512L), quote = '"')
      else if (typeof(value) == "double") value <- formatC(value, digits = 6L, format = "g", width = 1L)
      else if (typeof(value) == "complex") value <- ifelse(is.na(value), "NA", paste0(
        formatC(Re(value), digits = 6L, format = "g", width = 1L),
        ifelse(Im(value) < 0, "", "+"), formatC(Im(value), digits = 6L, format = "g", width = 1L), "i"))
      else value <- as.character(value)
      paste0(paste(value, collapse = ", "), if (n > items) ", ..." else "")
    } else if (type %in% c("language", "symbol", "expression", "pairlist"))
      paste(deparse(x, width.cutoff = 80L, nlines = 2L), collapse = " ") else
      paste0("<", type, if (type == "list") paste0("; ", length(.explorer_raw(x)), " children") else "", ">")
  out <- .panel_slice(.safe_text(out), 0L, 120L)
  stopifnot("brief preview must be scalar safe text" = is.character(out) && length(out) == 1L && !is.na(out))
  out
}

#' Describe the Children of a Node Without Evaluating Bindings
#' @param x An object.
#' @param view Children or attributes.
#' @return Child names, count, access kind, and ordinary container.
#' @noRd
.explorer_descriptor <- function(x, view = "children") {
  stopifnot("view must select children or attributes" = view %in% c("children", "attributes"))
  if (view == "attributes") {
    values <- attributes(x)
    if (is.null(values)) values <- list()
    kind <- "element"
    labels <- names(values)
  } else if (is.environment(x)) {
    values <- x
    kind <- "binding"
    labels <- ls(x, all.names = TRUE, sorted = TRUE)
  } else if (isS4(x)) {
    values <- x
    kind <- "slot"
    labels <- methods::slotNames(x)
  } else if (typeof(x) == "closure") {
    values <- list(formals = formals(x), body = body(x), environment = environment(x))
    kind <- "element"
    labels <- names(values)
  } else if (typeof(x) %in% c("list", "expression", "pairlist", "language")) {
    values <- .explorer_raw(x)
    if (typeof(values) %in% c("pairlist", "language")) values <- as.list(values)
    kind <- "element"
    labels <- attr(values, "names", exact = TRUE)
  } else {
    values <- list()
    labels <- character()
    kind <- "element"
  }
  count <- if (kind %in% c("binding", "slot")) length(labels) else length(values)
  out <- list(kind = kind, values = values, names = labels, count = as.double(count),
    template = if (view == "children" && kind == "element") .explorer_template(x) else NULL)
  stopifnot("child count must be nonnegative" = is.finite(out$count) && out$count >= 0,
            "child names must be absent or aligned" = is.null(out$names) || length(out$names) == out$count)
  out
}

#' Retrieve an Explicitly Chosen Child Without Evaluating Special Bindings
#' @param descriptor Child-access descriptor.
#' @param index One-based child index.
#' @return A list with ok, value, and reason fields.
#' @noRd
.explorer_child <- function(descriptor, index) {
  stopifnot("descriptor must describe children" = is.list(descriptor) &&
              descriptor$kind %in% c("binding", "slot", "element"),
            "index must identify a child" = .is_count(index, 1L) && index <= descriptor$count)
  out <- tryCatch({
    if (descriptor$kind == "binding") {
      name <- descriptor$names[[index]]
      state <- binding_info(descriptor$values, name)[[1L]]
      if (state != "value") return(list(ok = FALSE, value = NULL,
        reason = paste0(state, " binding; not evaluated")))
      value <- get(name, descriptor$values, inherits = FALSE)
    } else if (descriptor$kind == "slot") value <- methods::slot(descriptor$values, descriptor$names[[index]])
    else value <- .explorer_restore(descriptor$values[[index]], descriptor$template)
    list(ok = TRUE, value = value, reason = "")
  }, error = function(e) list(ok = FALSE, value = NULL, reason = .safe_text(conditionMessage(e))))
  stopifnot("child access must report success or a reason" = .is_flag(out$ok) &&
              is.character(out$reason) && length(out$reason) == 1L)
  out
}

#' Create One Cached Explorer Node
#' @param x Value to inspect.
#' @param label Breadcrumb component.
#' @param limit Maximum children per batch.
#' @param filter Initial name filter.
#' @param sizes Show cached sizes for visible child records.
#' @return A node list; its cache is local to the explorer session.
#' @noRd
.explorer_node <- function(x, label, limit, filter = "", sizes = FALSE) {
  stopifnot("label must be text" = .is_text(label), "limit must be positive" = .is_count(limit, 1L),
            "filter must be a scalar string" = is.character(filter) && length(filter) == 1L && !is.na(filter),
            "sizes must be logical" = .is_flag(sizes))
  cache <- new.env(parent = emptyenv())
  cache$descriptors <- list()
  cache$summaries <- new.env(parent = emptyenv())
  cache$sizes <- new.env(parent = emptyenv())
  cache$size_revision <- 0L
  cache$summary_order <- character()
  cache$frames <- new.env(parent = emptyenv())
  out <- list(value = x, label = .explorer_text(label), limit = as.integer(limit),
    view = if (.explorer_leaf(x)) "preview" else "children", query = filter,
    offset = 0L, cursor = 1L, top = 1L, show_sizes = sizes,
    meta = .explorer_meta(x, sizes = TRUE), cache = cache)
  stopifnot("new explorer nodes must start at their first child" = out$cursor == 1L && out$offset == 0L)
  out
}

#' Create a Structural Explorer State
#' @param x Root value.
#' @param label Root breadcrumb.
#' @param limit Children per batch.
#' @param filter Initial name filter.
#' @param sizes Show cached sizes for visible child records.
#' @return A navigable state list.
#' @noRd
.explorer_state <- function(x, label = "object", limit = 200L, filter = "", sizes = FALSE) {
  stopifnot("label must be text" = .is_text(label), "limit must be positive" = .is_count(limit, 1L))
  out <- list(stack = list(.explorer_node(x, label, limit, filter, sizes)), searching = FALSE,
    search_before = list(), status = "", quit = FALSE, help = FALSE, help_top = 1L,
    last_click = NULL)
  stopifnot("explorer must have one root and remain open" = length(out$stack) == 1L && !out$quit)
  out
}

#' Get a Cached Child Descriptor for a Node View
#' @param node Explorer node.
#' @return A child descriptor.
#' @noRd
.explorer_describe <- function(node) {
  stopifnot("node must have a structural view" = is.list(node) && node$view %in% c("children", "attributes"))
  descriptor <- node$cache$descriptors[[node$view]]
  if (is.null(descriptor)) {
    descriptor <- .explorer_descriptor(node$value, node$view)
    node$cache$descriptors[[node$view]] <- descriptor
  }
  stopifnot("descriptor must contain a nonnegative count" = is.list(descriptor) && descriptor$count >= 0)
  descriptor
}

#' Load a Bounded Page of Child Metadata
#' @param node Explorer node.
#' @return Data, child indices, and paging counts.
#' @noRd
.explorer_load <- function(node) {
  stopifnot("node must have a structural view" = is.list(node) && node$view %in% c("children", "attributes"))
  key <- list(node$view, node$query, node$offset)
  if (identical(node$cache$load_key, key)) return(node$cache$load)
  descriptor <- .explorer_describe(node)
  matches <- NULL
  if (nzchar(node$query)) {
    if (!is.null(descriptor$names)) matches <- which(stringi::stri_detect_fixed(
      descriptor$names, node$query, opts_fixed = stringi::stri_opts_fixed(case_insensitive = TRUE)))
    if (!length(matches) && grepl("^[0-9]+$", node$query)) {
      number <- suppressWarnings(as.double(node$query))
      if (is.finite(number) && number >= 1 && number <= descriptor$count) matches <- number
    }
    matched <- length(matches)
  } else matched <- descriptor$count
  offset <- min(node$offset, max(0, matched - 1L))
  positions <- if (matched > 0) offset + seq_len(min(node$limit, matched - offset)) else integer()
  indices <- if (nzchar(node$query)) matches[positions] else positions
  empty <- data.frame(name = character(), type = character(), extent = character(), size = character(),
                       preview = character(), stringsAsFactors = FALSE)
  rows <- lapply(indices, function(index) {
    cache_key <- paste0(node$view, ":", index)
    cached <- get0(cache_key, node$cache$summaries, inherits = FALSE)
    if (!is.null(cached)) return(cached)
    name <- if (!is.null(descriptor$names) && !is.na(descriptor$names[[index]]) &&
                 nzchar(descriptor$names[[index]])) descriptor$names[[index]] else paste0("[[", index, "]]")
    child <- .explorer_child(descriptor, index)
    row <- if (!child$ok) data.frame(name = .explorer_text(name), type = "binding", extent = "", size = "",
      preview = child$reason, stringsAsFactors = FALSE) else {
        meta <- .explorer_meta(child$value)
        data.frame(name = .explorer_text(name), type = meta$class, extent = meta$shape, size = "",
          preview = .explorer_brief(child$value), stringsAsFactors = FALSE)
      }
    assign(cache_key, row, node$cache$summaries)
    node$cache$summary_order <- c(node$cache$summary_order, cache_key)
    row
  })
  data <- if (length(rows)) do.call(rbind, rows) else empty
  if (length(node$cache$summary_order) > node$limit * 4L) {
    discard <- utils::head(node$cache$summary_order, -node$limit * 4L)
    rm(list = discard, envir = node$cache$summaries)
    sized <- discard[vapply(discard, exists, logical(1L), envir = node$cache$sizes, inherits = FALSE)]
    if (length(sized)) rm(list = sized, envir = node$cache$sizes)
    node$cache$summary_order <- utils::tail(node$cache$summary_order, node$limit * 4L)
  }
  rownames(data) <- NULL
  out <- list(data = data, indices = indices, total = descriptor$count,
               matched = matched, offset = offset, descriptor = descriptor)
  node$cache$load_key <- key
  node$cache$load <- out
  stopifnot("child metadata must stay within the page budget" = nrow(out$data) <= node$limit &&
              nrow(out$data) == length(out$indices))
  out
}

#' Cache Sizes Only for Child Records Visible in the Current View
#' @param node Explorer node.
#' @param records One-based rows in the current bounded child batch.
#' @return Whether any new measurement was made.
#' @noRd
.explorer_measure <- function(node, records) {
  stopifnot("node must have a structural view" = node$view %in% c("children", "attributes"),
            "records must be numeric" = is.numeric(records))
  loaded <- .explorer_load(node)
  records <- unique(records[records >= 1L & records <= nrow(loaded$data)])
  changed <- vapply(records, function(record) {
    key <- paste0(node$view, ":", loaded$indices[[record]])
    if (exists(key, node$cache$sizes, inherits = FALSE)) return(FALSE)
    child <- .explorer_child(loaded$descriptor, loaded$indices[[record]])
    size <- if (child$ok) .explorer_meta(child$value, sizes = TRUE)$size else "--"
    if (!nzchar(size)) size <- "--"
    assign(key, size, node$cache$sizes)
    row <- get(key, node$cache$summaries, inherits = FALSE)
    row$size <- size
    assign(key, row, node$cache$summaries)
    node$cache$load$data$size[[record]] <- size
    TRUE
  }, logical(1L))
  if (any(changed)) node$cache$size_revision <- node$cache$size_revision + 1L
  out <- any(changed)
  stopifnot("measurement status must be logical" = .is_flag(out))
  out
}

#' Clip a Child-Table Cell with an Explicit Truncation Marker
#' @param text Text cells.
#' @param width Cell width.
#' @return Fixed-width plain cells.
#' @noRd
.explorer_fit <- function(text, width) {
  stopifnot("text must be nonmissing character" = is.character(text) && !anyNA(text),
            "width must be positive" = .is_count(width, 1L))
  long <- which(.panel_cells(text) > width)
  if (length(long)) text[long] <- vapply(text[long], function(line)
    if (width > 3L) paste0(.panel_slice(line, 0L, width - 3L), "...") else .panel_slice(line, 0L, width), character(1L))
  out <- .panel_fit(text, width)
  stopifnot("fitted cells must preserve their count" = length(out) == length(text))
  out
}

#' Format Child Metadata as a Responsive Table
#' @param data Child metadata.
#' @param width Content width.
#' @param sizes Include the cached size column.
#' @return Lines, physical-row record identifiers, and an optional column heading.
#' @noRd
.explorer_table <- function(data, width, sizes = FALSE) {
  stopifnot("data must have the child schema" = is.data.frame(data) &&
              identical(names(data), c("name", "type", "extent", "size", "preview")),
            "width must be positive" = .is_count(width, 1L), "sizes must be logical" = .is_flag(sizes))
  if (!nrow(data)) return(list(lines = "(No matching children)", records = 0L, heading = ""))
  if (width >= 74L) {
    a <- min(26L, max(14L, max(.panel_cells(data$name))))
    b <- min(20L, max(12L, max(.panel_cells(data$type))))
    c <- 15L
    size_width <- if (sizes) 10L else 0L
    gaps <- if (sizes) 8L else 6L
    d <- max(8L, width - a - b - c - size_width - gaps)
    if (a + b + c + d + size_width + gaps > width) a <- width - b - c - d - size_width - gaps
    values <- list(.explorer_fit(data$name, a), .explorer_fit(data$type, b), .explorer_fit(data$extent, c))
    headings <- c(.panel_fit("Name", a), .panel_fit("Type / class", b), .panel_fit("Shape / length", c))
    if (sizes) {
      values <- c(values, list(.explorer_fit(ifelse(nzchar(data$size), data$size, "..."), size_width)))
      headings <- c(headings, .panel_fit("Size", size_width))
    }
    lines <- do.call(paste, c(values, list(.explorer_fit(data$preview, d)), sep = "  "))
    heading <- paste(c(headings, .panel_fit("Value", d)), collapse = "  ")
    out <- list(lines = lines, records = seq_len(nrow(data)), heading = heading)
  } else {
    detail <- paste0(data$type, ifelse(nzchar(data$extent), paste0("  ", data$extent), ""),
                      if (sizes) paste0("  ", ifelse(nzchar(data$size), data$size, "...")) else "",
                      ifelse(nzchar(data$preview), paste0("  |  ", data$preview), ""))
    out <- .table_format(data.frame(name = data$name, detail = detail, stringsAsFactors = FALSE), width)
    out$heading <- "Name / type / shape / value"
  }
  stopifnot("child table must fit the content width" = all(.panel_cells(out$lines) <= width),
            "record identifiers must align with rows" = length(out$records) == length(out$lines))
  out
}

#' Build a Safe Bounded Preview of the Opened Node
#' @param x An object.
#' @param width Content width.
#' @param rows Preview budget.
#' @param offset Number of matching values before the current batch.
#' @param selection Optional cached preview selection.
#' @param query Literal value filter, or an empty string.
#' @return Plain preview lines.
#' @noRd
.explorer_preview <- function(x, width, rows, offset = 0L, selection = NULL, query = "") {
  stopifnot("width must be positive" = .is_count(width, 1L), "rows must be positive" = .is_count(rows, 1L),
    "offset must be nonnegative" = .is_count(offset, 0L),
    "query must be one string" = is.character(query) && length(query) == 1L && !is.na(query))
  if (is.null(selection)) selection <- .explorer_preview_load(list(view = "preview", value = x,
    limit = rows, offset = offset, query = query, cache = new.env(parent = emptyenv())))
  out <- if (!is.null(selection) && selection$spec$kind == "table") {
    .explorer_preview_table(selection, width)
  } else if (!is.null(selection)) {
    spec <- selection$spec
    indices <- selection$indices
    if (!length(indices) && nzchar(query)) "(No matching values)" else if (spec$count <= 1L) {
      value <- if (!is.null(x) && !spec$count) paste0(typeof(spec$raw), "(0)") else .explorer_brief(x)
      c(value, "", "Atomic value: no further children.")
    } else {
      labels <- paste0("[", formatC(indices, format = "f", digits = 0L, width = 1L), "]")
      if (!is.null(spec$names)) {
        names <- spec$names[indices]
        names[is.na(names)] <- "<NA>"
        labels <- paste(labels, .explorer_text(names))
      }
      values <- .explorer_preview_column(spec, 1L, indices)
      c(.table_format(data.frame(index = labels, value = values, stringsAsFactors = FALSE), width)$lines,
        if (selection$matched > rows) paste0("Elements ", selection$offset + 1L, "-",
          selection$offset + length(indices), " of ", selection$matched,
          ". Use [ / ] for another preview batch."))
    }
  } else {
    lines <- if (is.function(x)) deparse(x, width.cutoff = max(20L, min(500L, width)), nlines = rows)
      else if (is.environment(x)) c("Environment values are opened individually from Children.",
        "Active bindings and promises remain unevaluated.") else if (isS4(x))
          c("S4 slots are listed in Children.", "Open a slot to inspect its ordinary value.") else
            c(.explorer_brief(x), "Use Children to inspect individual elements; switch views to inspect attributes.")
    if (nzchar(query)) lines <- lines[stringi::stri_detect_fixed(.safe_text(lines), query,
      opts_fixed = stringi::stri_opts_fixed(case_insensitive = TRUE))]
    if (length(lines)) lines else "(No matching preview text)"
  }
  if (!length(out)) out <- if (is.null(x)) "NULL" else "(empty object)"
  out <- unlist(lapply(.safe_text(out), function(line) .condition_wrap(line, max(4L, width), continuation = "")), use.names = FALSE)
  if (width < 4L) out <- .panel_fit(out, width)
  out <- utils::head(out, rows * 4L + 12L)
  stopifnot("safe preview must be bounded nonmissing text" = is.character(out) && !anyNA(out) &&
    length(out) <= rows * 4L + 12L)
  out
}

#' Prepare Cached Display Rows for the Active Explorer View
#' @param node Explorer node.
#' @param width Content width.
#' @return Lines, record mapping, and optional table heading.
#' @noRd
.explorer_content <- function(node, width) {
  stopifnot("node must be a list" = is.list(node), "width must be positive" = .is_count(width, 1L))
  key <- list(node$view, node$query, node$offset, width, node$show_sizes, node$cache$size_revision)
  if (identical(node$cache$content_key, key)) return(node$cache$content)
  out <- if (node$view == "preview") {
    selection <- .explorer_preview_load(node)
    lines <- .explorer_preview(node$value, width, node$limit, node$offset,
      selection = selection, query = node$query)
    list(lines = lines, records = seq_along(lines), heading = if (.explorer_leaf(node$value))
      "Atomic value: no further children" else if (is.null(selection))
        "Bounded preview text; custom methods are not called" else
          "Underlying values; filter checks every row and column",
      matches = if (nzchar(node$query)) .panel_matches(lines, node$query) else NULL)
  } else if (node$view == "children" && .explorer_leaf(node$value)) {
    lines <- .condition_wrap(c("Atomic value: no further children.",
      .explorer_brief(node$value), "Use Preview or Attributes for more detail."), max(4L, width))
    if (width < 4L) lines <- .panel_fit(lines, width)
    list(lines = lines, records = rep(0L, length(lines)), heading = "Leaf value")
  } else .explorer_table(.explorer_load(node)$data, width, node$show_sizes)
  node$cache$content_key <- key
  node$cache$content <- out
  stopifnot("explorer content must contain lines" = is.character(out$lines) && length(out$lines) > 0L)
  out
}

#' Return the Structural Explorer Shortcut Reference
#' @param config Configuration, including effective key bindings.
#' @param width Available content width.
#' @return Plain help lines.
#' @noRd
.explorer_help <- function(config = list(), width = 80L) {
  stopifnot("config must be a list" = is.list(config), "width must be positive" = .is_count(width, 1L))
  keys <- c(.viewer_keys(config, c("submit", "right")),
    .viewer_keys(config, c("left", "backspace")), .viewer_keys(config, c("complete", "complete_previous")),
    .viewer_keys(config, c("up", "down")), .viewer_keys(config, c("scroll_up", "scroll_down")),
    "[ / ]", "s", .viewer_keys(config, c("line_start", "line_end")), "/",
    .viewer_keys(config, c("submit", "escape")), "Mouse wheel", "Click", "Double-click",
    "Drag / Ctrl+drag", "Right-click",
    .viewer_keys(config, "help"), paste0("q / ", .viewer_keys(config, "escape")))
  descriptions <- c("Open the selected child.", "Return to the parent object.",
    "Switch Children, Preview, Attributes.", "Select a record or move a preview line.",
    "Move one visible page.", "Previous or next bounded batch, including atomic previews.",
    "Toggle cached sizes for visible child records.", "First or last record.",
    "Filter names in Children/Attributes; values in Preview.", "Apply the filter or restore the previous filter and position.",
    "Move three records.", "Select a child.", "Open a child.",
    "Select and copy visible text; Ctrl selects a rectangle.",
    "Paste into the current view's filter.",
    "Toggle this shortcut reference.", "Close the explorer.")
  out <- c(.table_format(data.frame(key = keys, action = descriptions, stringsAsFactors = FALSE), width)$lines,
    "", .condition_wrap(c("Promises and active bindings are never evaluated.",
      "Atomic vectors open as Preview leaves; opening them never creates element children.",
      "Attributes remain inspectable; [ / ] pages through matching rows or atomic values.",
      "Preview filters are literal and case-insensitive; Ctrl+U clears the query.",
      "Tables search every row and column, including values beyond the visible batch.",
      "Primitive values are searched before display clipping; list cells use safe summaries.",
      "Other objects filter their bounded preview text without calling custom methods.",
      "Approximate sizes can count shared storage more than once and exclude referenced environments."),
      max(4L, width), continuation = ""))
  if (width < 4L) out <- .panel_fit(out, width)
  stopifnot("explorer shortcuts must be nonmissing text" = is.character(out) && !anyNA(out))
  out
}

#' Compose a Structural Explorer Frame
#' @param state Explorer state.
#' @param config Configuration.
#' @param size Terminal dimensions.
#' @return Styled rows with navigation hit-test attributes.
#' @noRd
.explorer_frame <- function(state, config, size) {
  stopifnot("state must have an active node" = is.list(state) && length(state$stack) >= 1L)
  node <- utils::tail(state$stack, 1L)[[1L]]
  width <- max(1L, as.integer(size[["width"]]) - 3L)
  breadcrumb <- paste(vapply(state$stack, `[[`, character(1L), "label"), collapse = " / ")
  if (.panel_cells(breadcrumb) > max(10L, width - 18L)) breadcrumb <- paste0("... ",
    .panel_slice(breadcrumb, max(0L, .panel_cells(breadcrumb) - max(10L, width - 22L)), max(10L, width - 22L)))
  title <- paste0("Object explorer: ", breadcrumb)
  metadata <- paste(Filter(nzchar, c(paste0(node$meta$class, " <", node$meta$type, ">"),
    node$meta$shape, node$meta$size)), collapse = "  |  ")
  views <- c(children = "Children", preview = "Preview", attributes = "Attributes")
  tabs <- paste(ifelse(names(views) == node$view, paste0("[", views, "]"), views), collapse = "   ")
  leaf <- node$view != "attributes" && .explorer_leaf(node$value)
  preview <- if (node$view == "preview") .explorer_preview_load(node) else NULL
  pageable <- !is.null(preview) && preview$matched > node$limit
  hint_key <- list(config$keybindings, width, config$theme$colors, config$color, leaf, pageable, node$view)
  if (!identical(node$cache$hint_key, hint_key)) {
    hints <- if (node$view == "preview" || leaf) stats::setNames(
      c("parent", "view", "filter", if (pageable) "batch", "keys", "close"),
      c(.viewer_keys(config, "left"), .viewer_keys(config, "complete"), "/",
        if (pageable) "[/]", .viewer_keys(config, "help"), "q")) else
      stats::setNames(c("open", "parent", "view", "filter", "keys", "close"),
        c(.viewer_keys(config, "submit"), .viewer_keys(config, "left"), .viewer_keys(config, "complete"),
          "/", .viewer_keys(config, "help"), "q"))
    node$cache$caption <- .footer_hints(hints, config, width)
    node$cache$filter_hint <- paste0("  ", .footer_hints(stats::setNames(c("apply", "cancel"),
      c(.viewer_keys(config, "submit"), .viewer_keys(config, "escape"))), config, width))
    node$cache$help_hint <- .footer_hints(stats::setNames("return to object",
      paste0(.viewer_keys(config, c("help", "escape")), "/q")), config, width)
    node$cache$hint_key <- hint_key
  }
  if (state$help && !identical(node$cache$help_key, hint_key)) {
    node$cache$help_lines <- .explorer_help(config, width)
    node$cache$help_key <- hint_key
  }
  content <- if (state$help) list(lines = node$cache$help_lines, records = seq_along(node$cache$help_lines),
                                  heading = "Explorer keyboard and mouse shortcuts") else .explorer_content(node, width)
  # Content rows reserve two cells for the cursor. The heading reserves one
  # additional cell here because panel headers already carry one leading cell.
  headers <- c(metadata, tabs, paste0(" ", content$heading))
  layout <- .panel_layout(size, 1L + length(headers))
  selected <- if (state$help) state$help_top else node$cursor
  active <- which(content$records == selected)
  if (!length(active)) active <- 1L
  top <- if (state$help) state$help_top else node$top
  top <- max(1L, min(top, length(content$lines)))
  if (min(active) < top) top <- min(active)
  if (max(active) >= top + layout$page) top <- max(1L, max(active) - layout$page + 1L)
  visible <- seq.int(top, min(length(content$lines), top + layout$page - 1L))
  if (!state$help && !state$searching && node$view != "preview" && node$show_sizes &&
      .explorer_measure(node, content$records[visible])) {
    content <- .explorer_content(node, width)
    active <- which(content$records == selected)
    if (!length(active)) active <- 1L
    if (min(active) < top) top <- min(active)
    if (max(active) >= top + layout$page) top <- max(1L, max(active) - layout$page + 1L)
  }
  loaded <- if (node$view != "preview") .explorer_load(node) else NULL
  footer <- if (state$help) node$cache$help_hint else if (!is.null(loaded))
    paste0("Items ", if (length(loaded$indices)) loaded$offset + 1L else 0L, "-",
      loaded$offset + length(loaded$indices), " of ", loaded$matched,
      if (nzchar(node$query)) paste0(" matching '", node$query, "'") else "",
      "  selected ", if (nrow(loaded$data)) min(node$cursor, nrow(loaded$data)) else 0L,
      "  ", .footer_hints(c("[/]" = "batch", s = paste("sizes", if (node$show_sizes) "on" else "off")),
        config, width)) else if (!is.null(preview)) paste0(
          if (preview$spec$kind == "table") "Rows " else "Values ",
          if (length(preview$indices)) preview$offset + 1L else 0L, "-",
          preview$offset + length(preview$indices), " of ", preview$matched,
          if (nzchar(node$query)) paste0(" (", preview$total, " total) matching '", node$query, "'") else "") else
            paste0("Preview line ", node$cursor, "/", length(content$lines),
              if (nzchar(node$query)) paste0(" matching '", node$query, "' (bounded text)") else "")
  if (nzchar(state$status)) footer <- state$status
  caption <- if (state$searching) paste0(style_text(
    if (node$view == "preview") "Filter values: " else "Filter names: ", "accent", config$theme, config$color),
    node$query, node$cache$filter_hint) else node$cache$caption
  frame <- .panel_frame(content$lines, title, config, size, cursor = min(active), top = top,
    caption = caption, footer = footer, cache = node$cache$frames, header = headers,
    active_rows = if (state$help) integer() else active,
    matches = if (state$help) NULL else content$matches)
  hit <- rep(NA_integer_, length(frame))
  shown <- seq.int(top, min(length(content$lines), top + layout$page - 1L))
  hit[layout$header + seq_along(shown)] <- content$records[shown]
  attr(frame, "explorer_records") <- hit
  attr(frame, "explorer_top") <- top
  attr(frame, "explorer_page") <- max(1L, length(unique(content$records[shown][content$records[shown] > 0L])))
  stopifnot("explorer hit map must match the frame" = length(attr(frame, "explorer_records")) == length(frame))
  frame
}

#' Apply an Explorer Event Without Mutating Inspected Data
#' @param state Explorer state.
#' @param event Native or synthetic terminal event.
#' @param config Configuration.
#' @param page Visible records per page.
#' @param hit Record under a mouse press, or NA.
#' @param now Monotonic elapsed time used for double-click detection.
#' @return Updated explorer state.
#' @noRd
.explorer_apply <- function(state, event, config, page = 10L, hit = NA_integer_, now = proc.time()[["elapsed"]]) {
  stopifnot("state must have an active node" = is.list(state) && length(state$stack) >= 1L,
            "event must be a list" = is.list(event), "page must be positive" = .is_count(page, 1L))
  depth <- length(state$stack)
  node <- state$stack[[depth]]
  key <- .viewer_action(event, config)
  text <- if (is.null(event$text)) "" else event$text
  ctrl <- "ctrl" %in% event$modifiers
  if (!identical(event$type, "mouse")) state$last_click <- NULL
  state$status <- ""
  if (identical(event$type, "eof")) { state$quit <- TRUE; return(state) }
  if (state$help && identical(event$type, "paste")) state$help <- FALSE
  if (state$help) {
    if (key %in% c("help", "escape") || identical(text, "q")) state$help <- FALSE
    count <- if (length(node$cache$help_lines)) length(node$cache$help_lines) else length(.explorer_help(config))
    if (key == "down" || identical(event$key, "wheeldown")) state$help_top <- min(count, state$help_top + 1L)
    if (key == "up" || identical(event$key, "wheelup")) state$help_top <- max(1L, state$help_top - 1L)
    if (key == "pagedown") state$help_top <- min(count, state$help_top + page)
    if (key == "pageup") state$help_top <- max(1L, state$help_top - page)
    if (key == "home") state$help_top <- 1L
    if (key == "end") state$help_top <- count
    return(state)
  }
  if (state$searching) {
    if (key == "escape" || (ctrl && key == "c")) {
      node[names(state$search_before)] <- state$search_before
      state$searching <- FALSE
      state$stack[[depth]] <- node
      return(state)
    } else if (key == "enter") state$searching <- FALSE
    else if (key == "backspace") node$query <- paste(utils::head(
      stringi::stri_split_boundaries(node$query, type = "character")[[1L]], -1L), collapse = "")
    else if (ctrl && identical(event$key, "u")) node$query <- ""
    else if (event$type %in% c("text", "paste")) node$query <- substr(paste0(node$query,
      gsub("[\r\n\t]", " ", .safe_text(text))), 1L, 200L)
    node$offset <- 0L
    node$cursor <- node$top <- 1L
    state$stack[[depth]] <- node
    return(state)
  }
  if (key == "help") { state$help <- TRUE; state$help_top <- 1L; return(state) }
  if (identical(event$type, "paste")) {
    state$searching <- TRUE
    state$search_before <- node[c("query", "offset", "cursor", "top")]
    node$query <- substr(gsub("[\r\n\t]", " ", .safe_text(text)), 1L, 200L)
    node$offset <- 0L
    node$cursor <- node$top <- 1L
    state$stack[[depth]] <- node
    return(state)
  }
  if (identical(text, "s") && node$view != "preview") {
    node$show_sizes <- !node$show_sizes
    state$stack[[depth]] <- node
    return(state)
  }
  if (key == "escape" || identical(text, "q") || (ctrl && key == "c")) {
    state$quit <- TRUE
    return(state)
  }
  if (key %in% c("left", "backspace")) {
    if (depth > 1L) state$stack <- state$stack[-depth] else state$status <- "Already at the root object."
    state$last_click <- NULL
    return(state)
  }
  if (key %in% c("enter", "right") && node$view != "attributes" && .explorer_leaf(node$value)) {
    state$status <- "Atomic value: no further children. Attributes remain available."
    return(state)
  }
  if (key %in% c("tab", "backtab")) {
    views <- c("children", "preview", "attributes")
    index <- match(node$view, views)
    node$view <- views[[(index - 1L + if (key == "backtab") -1L else 1L) %% 3L + 1L]]
    node$cursor <- node$top <- 1L
    node$offset <- 0L
    node$query <- ""
    state$stack[[depth]] <- node
    return(state)
  }
  if (identical(text, "/")) {
    state$searching <- TRUE
    state$search_before <- node[c("query", "offset", "cursor", "top")]
    node$query <- ""
    node$offset <- 0L
    node$cursor <- node$top <- 1L
    state$stack[[depth]] <- node
    return(state)
  }
  data <- if (node$view != "preview") .explorer_load(node) else NULL
  preview <- if (node$view == "preview") .explorer_preview_load(node) else NULL
  preview_total <- if (is.null(preview)) 0L else preview$matched
  preview_width <- if (is.list(node$cache$content_key)) node$cache$content_key[[4L]] else 80L
  count <- if (is.null(data)) {
    length(.explorer_content(node, preview_width)$lines)
  } else nrow(data$data)
  if (identical(event$type, "mouse")) {
    if (event$key %in% c("wheelup", "wheeldown")) {
      key <- if (event$key == "wheelup") "up" else "down"
      page <- 3L
    } else if (identical(event$action, "press") && identical(event$button, "left") &&
               !is.na(hit) && hit >= 1L && hit <= count) {
      node$cursor <- as.integer(hit)
      location <- list(depth, node$view, node$query, node$offset, node$cursor)
      double <- !is.null(state$last_click) && identical(state$last_click$location, location) &&
        now - state$last_click$time <= 0.45
      state$last_click <- list(location = location, time = now)
      key <- if (double) "enter" else ""
    } else key <- ""
  }
  if (key %in% c("enter", "right") && !is.null(data) && count > 0L) {
    if (depth >= 32L) state$status <- "Maximum navigation depth reached; return to a parent." else {
      index <- data$indices[[min(node$cursor, count)]]
      child <- .explorer_child(data$descriptor, index)
      if (child$ok) {
        state$stack[[depth]] <- node
        state$stack[[depth + 1L]] <- .explorer_node(child$value,
          data$data$name[[min(node$cursor, count)]], node$limit, sizes = node$show_sizes)
        state$last_click <- NULL
        return(state)
      }
      state$status <- paste0("Cannot open: ", child$reason, ".")
    }
  }
  step <- if (identical(event$type, "mouse") && event$key %in% c("wheelup", "wheeldown")) 3L else 1L
  move <- if (key == "up" || identical(text, "k")) -step else if (key == "down" || identical(text, "j")) step else
    if (key == "pageup") -page else if (key == "pagedown") page else 0L
  next_batch <- identical(text, "]") || (move > 0L && node$cursor >= count)
  previous_batch <- identical(text, "[") || (move < 0L && node$cursor <= 1L)
  if (!is.null(data) && next_batch && node$offset + count < data$matched) {
    node$offset <- node$offset + node$limit
    node$cursor <- node$top <- 1L
  } else if (!is.null(data) && previous_batch && node$offset > 0L) {
    node$offset <- max(0L, node$offset - node$limit)
    node$cursor <- node$limit
    node$top <- 1L
  } else if (is.null(data) && next_batch && node$offset + node$limit < preview_total) {
    node$offset <- node$offset + node$limit
    node$cursor <- node$top <- 1L
  } else if (is.null(data) && previous_batch && node$offset > 0L) {
    node$offset <- max(0L, node$offset - node$limit)
    node$cursor <- length(.explorer_content(node, preview_width)$lines)
    node$top <- 1L
  } else node$cursor <- max(1L, min(max(1L, count), node$cursor + move))
  if (key == "home") { node$offset <- 0L; node$cursor <- node$top <- 1L }
  if (key == "end") {
    if (!is.null(data) && data$matched > 0L) {
      node$offset <- floor((data$matched - 1L) / node$limit) * node$limit
      node$cursor <- data$matched - node$offset
    } else if (preview_total > 0L) {
      node$offset <- floor((preview_total - 1L) / node$limit) * node$limit
      node$cursor <- length(.explorer_content(node, preview_width)$lines)
    } else node$cursor <- max(1L, count)
  }
  state$stack[[depth]] <- node
  stopifnot("explorer navigation must preserve a valid root" = length(state$stack) >= 1L &&
              state$stack[[length(state$stack)]]$cursor >= 1L)
  state
}

#' Run the Structural Object Explorer
#' @param x Root value.
#' @param config Configuration.
#' @param rows Bounded page budget.
#' @param label Root breadcrumb.
#' @param filter Initial name filter.
#' @return Initial structural text invisibly.
#' @noRd
.object_explorer <- function(x, config, rows, label, filter = "") {
  stopifnot("config must be a console configuration" = inherits(config, "prismrepl_config"),
            "rows must be positive" = .is_count(rows, 1L), "label must be text" = .is_text(label))
  state <- .explorer_state(x, label, rows, filter, sizes = "memory" %in% config$status)
  node <- state$stack[[1L]]
  out <- c(paste0(label, "  ", node$meta$class, " <", node$meta$type, ">  ", node$meta$shape,
                   "  ", node$meta$size),
    .explorer_content(node, .viewer_width(config))$lines)
  session <- .viewer_start(config)
  on.exit(.viewer_finish(session, config), add = TRUE)
  if (is.null(session$handle)) {
    .emit(out, "output", config)
    return(invisible(out))
  }
  size <- term_size()
  previous <- NULL
  redraw <- TRUE
  frame <- NULL
  mouse <- .mouse_state()
  repeat {
    if (redraw) {
      frame <- .explorer_frame(state, config, size)
      state$stack[[length(state$stack)]]$top <- attr(frame, "explorer_top")
      previous <- .panel_draw(.mouse_paint(frame, mouse, config), previous, size, config)
      redraw <- FALSE
    }
    event <- term_read(session$handle, 50L)
    if (identical(event$type, "timeout")) next
    dispatched <- .mouse_dispatch(event, frame, mouse, config)
    mouse <- dispatched$selection
    if (nzchar(dispatched$status)) state$status <- dispatched$status
    redraw <- redraw || dispatched$redraw
    event <- dispatched$event
    if (is.null(event)) next
    if (identical(event$type, "resize")) { size <- term_size(); previous <- NULL; redraw <- TRUE; next }
    hits <- attr(frame, "explorer_records")
    hit <- if (identical(event$type, "mouse") && !is.null(event$y) && event$y >= 1L && event$y <= length(hits))
      hits[[event$y]] else NA_integer_
    state <- .explorer_apply(state, event, config, page = attr(frame, "explorer_page"), hit = hit)
    if (state$quit) break
    redraw <- TRUE
  }
  stopifnot("explorer output must be nonmissing text" = is.character(out) && !anyNA(out))
  invisible(out)
}
