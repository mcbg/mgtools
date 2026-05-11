
#' @export
copy_for_excel = \(dataset) dataset |> write.table(file = 'clipboard', sep = '\t', row.names = FALSE, col.names = TRUE)

#' @export
write_line_to_clipboard = \(text) writeLines(text = text, con = 'clipboard', sep = '') # writeClipboard adds \n
