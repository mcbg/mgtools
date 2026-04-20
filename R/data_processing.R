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
`%/%` = \(x, expr) x[grepl(x, expr, perl=TRUE)]
`%~%` = \(x, expr) grepl(x, expr, perl=TRUE)
`%s%` = \(datatable, group_expression) {
  captured_expression = substitute(datatable[, .N, group_expression])
  eval(captured_expression, parent.frame())
}


