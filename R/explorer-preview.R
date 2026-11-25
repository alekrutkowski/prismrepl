#' Describe Safe Value Rows for a Preview
#' @param x The inspected value.
#' @return A list describing table rows, vector elements, or bounded summary text.
#' @noRd
.explorer_preview_spec <- function(x) {
  raw <- .explorer_raw(x)
  dims <- attr(x, "dim", exact = TRUE)
  dataframe <- is.list(x) && !isS4(x) && inherits(x, "data.frame")
  kind <- if (dataframe || (!isS4(x) && is.atomic(raw) && length(dims) == 2L)) "table"
    else if (.explorer_leaf(x)) "vector" else "text"
  count <- if (dataframe) length(attr(x, "row.names", exact = TRUE)) else
    if (kind == "table") dims[[1L]] else if (kind == "vector") length(raw) else 0L
  columns <- if (dataframe) lapply(raw, .explorer_raw) else NULL
  names <- if (dataframe) attr(x, "names", exact = TRUE) else {
    dn <- attr(x, "dimnames", exact = TRUE)
    if (kind == "table" && length(dn) >= 2L) dn[[2L]] else attr(x, "names", exact = TRUE)
  }
  row_names <- if (dataframe) attr(x, "row.names", exact = TRUE) else {
    dn <- attr(x, "dimnames", exact = TRUE)
    if (kind == "table" && length(dn)) dn[[1L]] else NULL
  }
  out <- list(kind = kind, raw = raw, count = count,
    columns = if (dataframe) length(raw) else if (kind == "table") dims[[2L]] else 1L,
    dataframe = dataframe, values = columns, names = names, row_names = row_names,
    templates = if (dataframe) lapply(raw, .explorer_template) else .explorer_template(x))
  stopifnot("preview kind must be recognized" = out$kind %in% c("table", "vector", "text"),
    "preview row count must be nonnegative" = .is_count(out$count),
    "preview column count must be nonnegative" = .is_count(out$columns))
  out
}

#' Format Primitive Values Without Custom Method Dispatch
#' @param x Selected atomic values.
#' @param template Known factor or Date template, or NULL.
#' @return Safe, nonmissing strings, one per selected value.
#' @noRd
.explorer_preview_atom <- function(x, template = NULL) {
  stopifnot("values must be ordinary atomic values" = !isS4(x) && (is.atomic(x) || is.null(x)),
    "template must be a list or NULL" = is.null(template) || is.list(template))
  attributes(x) <- NULL
  count <- length(x)
  index <- NULL
  if (count > 1L && anyDuplicated(x)) {
    values <- unique(x)
    index <- match(x, values)
    x <- values
  }
  if (!is.null(template) && identical(template$class, "factor")) {
    labels <- rep(NA_character_, length(x))
    valid <- !is.na(x) & is.finite(x) & x == trunc(x) & x >= 1L & x <= length(template$levels)
    labels[valid] <- template$levels[x[valid]]
    out <- encodeString(labels, quote = '"')
  } else if (!is.null(template) && identical(template$class, "Date")) {
    # Invalid/nonfinite dates can warn during the base integer conversion.
    # Their missing display labels are normalized below without changing options.
    out <- tryCatch(suppressWarnings(base::format.Date(structure(x, class = "Date"), format = "%Y-%m-%d")),
      error = function(e) formatC(x, digits = 6L, format = "g", width = 1L))
  } else if (is.character(x)) out <- encodeString(x, quote = '"')
  else if (typeof(x) == "double") out <- formatC(x, digits = 6L, format = "g", width = 1L)
  else if (typeof(x) == "complex") out <- ifelse(is.na(x), "NA", paste0(
    formatC(Re(x), digits = 6L, format = "g", width = 1L),
    ifelse(Im(x) < 0, "", "+"), formatC(Im(x), digits = 6L, format = "g", width = 1L), "i"))
  else out <- as.character(x)
  out[is.na(out)] <- "NA"
  out <- .explorer_text(out)
  if (!is.null(index)) out <- out[index]
  stopifnot("formatted values must be safe and aligned" = is.character(out) &&
    !anyNA(out) && length(out) == count)
  out
}

#' Format Selected Rows of One Safe Preview Column
#' @param spec Preview descriptor.
#' @param column Column number, one for a vector.
#' @param indices Original row or element indices.
#' @return One safe string for each selected row.
#' @noRd
.explorer_preview_column <- function(spec, column, indices) {
  stopifnot("descriptor must contain value rows" = spec$kind %in% c("table", "vector"),
    "column must identify a preview column" = .is_count(column, 1L) && column <= spec$columns,
    "indices must identify original rows" = is.numeric(indices) && !anyNA(indices) &&
      all(indices >= 1L & indices <= spec$count))
  if (!length(indices)) return(character())
  if (spec$kind == "vector") {
    out <- .explorer_preview_atom(spec$raw[indices], spec$templates)
  } else if (!spec$dataframe) {
    out <- .explorer_preview_atom(spec$raw[(column - 1L) * spec$count + indices], spec$templates)
  } else {
    value <- spec$values[[column]]
    template <- spec$templates[[column]]
    dims <- attr(value, "dim", exact = TRUE)
    if (!isS4(value) && is.atomic(value) && length(dims) >= 2L && dims[[1L]] == spec$count) {
      # A matrix/array column represents one compound cell per data-frame row.
      parts <- lapply(seq_len(prod(dims[-1L])), function(j)
        .explorer_preview_atom(value[(j - 1L) * spec$count + indices], template))
      out <- if (length(parts)) do.call(paste, c(parts, sep = ", ")) else rep("", length(indices))
    } else if (!isS4(value) && is.atomic(value)) {
      out <- .explorer_preview_atom(value[indices], template)
    } else {
      out <- vapply(indices, function(i) {
        selected <- tryCatch(if (isS4(value) || is.environment(value)) NULL else value[[i]],
          error = function(e) NULL)
        .explorer_brief(selected, 1L)
      }, character(1L))
    }
  }
  stopifnot("preview cells must be safe and aligned" = is.character(out) &&
    !anyNA(out) && length(out) == length(indices))
  out
}

#' Match Literal Preview Values in Bounded Formatting Chunks
#' @param spec Preview descriptor.
#' @param query Nonempty literal query.
#' @param candidates Optional original indices from a shorter previous query.
#' @param chunk Maximum candidate rows formatted together.
#' @return Original row or element indices whose safe value text matches.
#' @noRd
.explorer_preview_matches <- function(spec, query, candidates = NULL, chunk = 4096L) {
  stopifnot("descriptor must contain value rows" = spec$kind %in% c("table", "vector"),
    "query must be nonempty text" = .is_text(query),
    "candidate rows must be absent or numeric" = is.null(candidates) || is.numeric(candidates),
    "chunk must be positive" = .is_count(chunk, 1L))
  count <- if (is.null(candidates)) spec$count else length(candidates)
  starts <- if (count) seq.int(1L, count, by = chunk) else integer()
  matches <- lapply(starts, function(first) {
    positions <- seq.int(first, min(count, first + chunk - 1L))
    indices <- if (is.null(candidates)) positions else candidates[positions]
    matched <- rep(FALSE, length(indices))
    for (column in seq_len(spec$columns)) {
      pending <- which(!matched)
      if (!length(pending)) break
      text <- .explorer_preview_column(spec, column, indices[pending])
      distinct <- unique(text)
      matched[pending] <- stringi::stri_detect_fixed(distinct, query,
        opts_fixed = stringi::stri_opts_fixed(case_insensitive = TRUE))[match(text, distinct)]
    }
    indices[matched]
  })
  out <- if (length(matches)) unlist(matches, use.names = FALSE) else integer()
  stopifnot("matched rows must be ordered original indices" = is.numeric(out) &&
    !anyNA(out) && all(out >= 1L & out <= spec$count) && !is.unsorted(out, strictly = TRUE))
  out
}

#' Load a Bounded Page of Filtered Preview Values
#' @param node Active preview node.
#' @return Descriptor, original indices, counts and effective offset, or NULL
#'   for objects represented by bounded summary text.
#' @noRd
.explorer_preview_load <- function(node) {
  stopifnot("node must select Preview" = is.list(node) && identical(node$view, "preview"))
  spec <- node$cache$preview_spec
  if (is.null(spec)) {
    spec <- .explorer_preview_spec(node$value)
    node$cache$preview_spec <- spec
  }
  if (identical(spec$kind, "text")) return(NULL)
  matches <- NULL
  if (nzchar(node$query)) {
    previous <- node$cache$preview_query
    if (identical(previous, node$query)) matches <- node$cache$preview_matches else {
      candidates <- if (!is.null(previous) && nzchar(previous) && startsWith(node$query, previous))
        node$cache$preview_matches else NULL
      matches <- .explorer_preview_matches(spec, node$query, candidates)
      node$cache$preview_query <- node$query
      node$cache$preview_matches <- matches
    }
  }
  matched <- if (nzchar(node$query)) length(matches) else spec$count
  offset <- min(node$offset, max(0L, matched - 1L))
  positions <- if (matched) offset + seq_len(min(node$limit, matched - offset)) else integer()
  out <- list(spec = spec, total = spec$count, matched = matched, offset = offset,
    indices = if (nzchar(node$query)) matches[positions] else positions)
  stopifnot("loaded preview rows must be bounded" = length(out$indices) <= node$limit &&
    all(out$indices >= 1L & out$indices <= out$total))
  out
}

#' Label Original Table Rows Without Renumbering Filter Results
#' @param spec Preview descriptor.
#' @param indices Original row indices.
#' @return Original positions with custom row labels when present.
#' @noRd
.explorer_preview_labels <- function(spec, indices) {
  stopifnot("indices must be numeric" = is.numeric(indices) && !anyNA(indices))
  positions <- formatC(indices, format = "f", digits = 0L, width = 1L)
  out <- positions
  if (!is.null(spec$row_names) && length(indices)) {
    labels <- as.character(spec$row_names[indices])
    custom <- !is.na(labels) & nzchar(labels) & labels != positions
    out[custom] <- paste0(positions[custom], ": ", .explorer_text(labels[custom]))
  }
  stopifnot("row labels must align with original positions" = is.character(out) &&
    !anyNA(out) && length(out) == length(indices))
  out
}

#' Format Bounded Table Rows with Their Original Identifiers
#' @param selection Loaded preview page.
#' @param width Content width.
#' @return Table heading, body and displayed-column scope.
#' @noRd
.explorer_preview_table <- function(selection, width) {
  stopifnot("selection must contain table rows" = selection$spec$kind == "table",
    "width must be positive" = .is_count(width, 1L))
  spec <- selection$spec
  indices <- selection$indices
  row_labels <- .explorer_preview_labels(spec, indices)
  row_width <- min(max(5L, nchar(as.character(spec$count)), .panel_cells(row_labels)),
    max(5L, width %/% 3L))
  nc <- min(8L, spec$columns, max(1L, (width - row_width - 1L) %/% 10L))
  columns <- seq_len(nc)
  labels <- if (is.null(spec$names)) paste0("V", columns) else spec$names[columns]
  labels[is.na(labels)] <- "<NA>"
  labels <- .explorer_text(labels)
  cell_width <- max(4L, (width - row_width - 1L) %/% max(1L, nc) - 1L)
  heading <- paste0(.explorer_fit("Row", row_width), " ",
    paste(.explorer_fit(labels, cell_width), collapse = " "))
  values <- lapply(columns, function(j)
    .explorer_fit(.explorer_preview_column(spec, j, indices), cell_width))
  body <- if (length(indices)) paste0(.explorer_fit(row_labels, row_width), " ",
    if (length(values)) do.call(paste, c(values, sep = " ")) else "") else "(No matching rows)"
  out <- c(heading, body, paste0("Showing ", length(indices), " of ", spec$count,
    " rows; ", nc, " of ", spec$columns, " columns. Filters check all columns."))
  stopifnot("table output must contain safe text" = is.character(out) && !anyNA(out))
  out
}
