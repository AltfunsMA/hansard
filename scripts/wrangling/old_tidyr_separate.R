separate <- function(data, col, into, sep = "[^[:alnum:]]+", remove = TRUE,
                     convert = FALSE, extra = "warn", fill = "warn", ...) {
  UseMethod("separate")
}
#' @export
separate.data.frame <- function(data, col, into, sep = "[^[:alnum:]]+",
                                remove = TRUE, convert = FALSE,
                                extra = "warn", fill = "warn", ...) {
  orig <- data
  
  var <- tidyselect::vars_pull(names(data), !! enquo(col))
  value <- as.character(data[[var]])
  
  if (!is.character(into)) {
    abort("`into` must be a character vector")
  }
  
  if (is.numeric(sep)) {
    l <- strsep(value, sep)
  } else if (is_character(sep)) {
    l <- str_split_fixed(value, sep, length(into), extra = extra, fill = fill)
  } else {
    abort("`sep` must be either numeric or character")
  }
  
  names(l) <- as_utf8_character(into)
  l <- l[!is.na(names(l))]
  if (convert) {
    l[] <- map(l, type.convert, as.is = TRUE)
  }
  
  # Insert into existing data frame
  data <- append_df(data, l, var, remove = remove)
  
  reconstruct_tibble(orig, data, if (remove) var else NULL)
}

strsep <- function(x, sep) {
  nchar <- stringi::stri_length(x)
  pos <- map(sep, function(i) {
    if (i >= 0) return(i)
    pmax(0, nchar + i)
  })
  pos <- c(list(0), pos, list(nchar))
  
  map(1:(length(pos) - 1), function(i) {
    stringi::stri_sub(x, pos[[i]] + 1, pos[[i + 1]])
  })
}
str_split_fixed <- function(value, sep, n, extra = "warn", fill = "warn") {
  if (extra == "error") {
    warn(glue(
      "`extra = \"error\"` is deprecated. \\
       Please use `extra = \"warn\"` instead"
    ))
    extra <- "warn"
  }
  
  extra <- arg_match(extra, c("warn", "merge", "drop"))
  fill <- arg_match(fill, c("warn", "left", "right"))
  
  n_max <- if (extra == "merge") n else -1L
  pieces <- stringi::stri_split_regex(value, sep, n_max)
  
  simp <- simplifyPieces(pieces, n, fill == "left")
  
  n_big <- length(simp$too_big)
  if (extra == "warn" && n_big > 0) {
    idx <- list_indices(simp$too_big)
    warn(glue("Expected {n} pieces. Additional pieces discarded in {n_big} rows [{idx}]."))
  }
  
  n_sml <- length(simp$too_sml)
  if (fill == "warn" && n_sml > 0) {
    idx <- list_indices(simp$too_sml)
    warn(glue("Expected {n} pieces. Missing pieces filled with `NA` in {n_sml} rows [{idx}]."))
  }
  
  simp$strings
}

list_indices <- function(x, max = 20) {
  if (length(x) > max) {
    x <- c(x[seq_len(max)], "...")
  }
  
  paste(x, collapse = ", ")
}