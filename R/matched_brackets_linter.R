#'
#' @describeIn linters checks that parentheses or curly-braces that begin on the
#'   same line also terminate on the same line.
#'
#' @importFrom xml2 xml_find_all as_xml_document xml_attr
#' @export
#'
matched_parens_linter <- function(source_file) {
  # short circuit on global expression
  if (is.null(source_file$parsed_content))
    return(list())

  left_paren_or_brace <- c("'('", "'{'")
  right_paren_or_brace <- c("')'", "'}'")

  content_df <<- source_file$parsed_content
  left_parens <- content_df[content_df$token %in% left_paren_or_brace,]
  right_parens <- content_df[content_df$token %in% right_paren_or_brace,]
  exprs <- content_df[content_df$token == "expr",]

  # exclud blocks whose parens close before opening a nested curly-braced expr
  control_tokens <- c("FUNCTION", "IF", "FOR", "WHILE")
  control_block_ids <- content_df[content_df$token %in% control_tokens, "parent"]
  exprs <- exprs[!exprs$id %in% control_block_ids,]

  # associate opening paren with each expression
  exprs <- merge(
    exprs,
    left_parens[,c("parent", "line1", "col1")],
    by.x = "id",
    by.y = "parent",
    suffixes = c("", ".lparen"))

  # associate closing paren with each expression
  exprs <- merge(
    exprs,
    right_parens[,c("parent", "line1", "col1")],
    by.x = "id",
    by.y = "parent",
    suffixes = c("", ".rparen"))

  # form pairs of nested expressions
  exprs <- merge(
    exprs,
    exprs,
    by.x = "id",
    by.y = "parent",
    suffixes = c("", ".par"))

  exprs$left_same_line <- with(exprs, line1.lparen == line1.lparen.par)
  exprs$right_same_line <- with(exprs, line1.rparen == line1.rparen.par)

  mismatched_brackets <- with(exprs, {
    left_same_line & (left_same_line != right_same_line)
  })

  mapply(
    Lint,
    line_number = exprs[mismatched_brackets, "line1.rparen"],
    column_number = exprs[mismatched_brackets, "col1.rparen"],
    MoreArgs = list(
      filename = source_file$filename,
      type = "style",
      message = paste0(
        "If a set of nested parentheses or curly braces open on the same ",
        "line, then they set pair should also close on the same line."),
      line = "",
      ranges = lapply(exprs[mismatched_brackets, "col1.rparen"], `+`, c(0, 1)),
      linter = "indentation_linter"),
    SIMPLIFY = FALSE)
}
