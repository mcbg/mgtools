#' @export
dataset_filter_columns = \(dataset, regex, invert=FALSE) {
  column_selection = grep(regex, names(dataset), invert = invert)
  new_columns = names(dataset)[column_selection]
  return(dataset[, ..new_columns])
}

#' @export
cross_join = \(x, ...) {
  y = data.table(...)
  x[, TEMPORARY.BY := 1]
  y[, TEMPORARY.BY := 1]
  merge(x,y, by='TEMPORARY.BY', allow.cartesian = TRUE)[, -'TEMPORARY.BY']
}

#' @export
`%[%` = \(x, expr) {
  if (is.vector(x) == TRUE) {
    return(x[grepl(expr, x, perl=TRUE)])
  }
  else if (is.data.frame(x) == TRUE) {
    rows = x |> apply(1, paste, collapse='|')
    rows_that_match = grepl(expr, rows, perl=TRUE)
    return(x[rows_that_match, ])
  }
}

#' @export
`%~%` = \(x, expr) grepl(expr, x, perl=TRUE)

#' @export
#' @import data.table
`%s%` = \(datatable, group_expression) {
  if (!data.table::is.data.table(datatable)) stop('datatable must be data.table')
  captured_expression = substitute(datatable[, .N, group_expression])
  eval(captured_expression, parent.frame())
}


