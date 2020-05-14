#'
#' @describeIn linters checks that function arguments wrap with a trailing
#'   indent with arguments spanning an 80 character limit or one argument per
#'   line.
#'
#' @importFrom xml2 xml_find_all as_xml_document xml_attr
#' @export
#'
func_call_argument_wrapping_linter <- function(length = 80L, indent = 2L) {
  function(source_file) {
    func_calls <- xml2::xml_find_all(
      source_file$xml_parsed_content,
      paste0(
        "//expr",
        "//SYMBOL_FUNCTION_CALL",
        "/..",
        "/parent::*"))

    lint <- lapply(
      func_calls,
      argument_wrapping_linter,
      source_file = source_file,
      indent = indent,
      length = length)

    flatten_lints(lint)
  }
}



#'
#' @describeIn linters checks that function parameters wrap with a preceding
#'   indent when they overflow the line width, or that all parameters are
#'   on provided on their own line if any parameters are multi-line expressions.
#'
#' @importFrom xml2 xml_find_all as_xml_document
#' @export
#'
func_header_argument_wrapping_linter <- function(length = 80L, indent = 2L) {
  func_header_argument_wrapping_linter <- function(source_file) {
    funcs <- xml2::xml_find_all(
      source_file$xml_parsed_content,
      "//FUNCTION/parent::*")

    lint <- lapply(funcs,
      argument_wrapping_linter,
      source_file = source_file,
      indent = indent,
      length = length)

    flatten_lints(lint)
  }
}




#'
#' Checks function header parameters:
#'
#'   # Lint (function header overflows an 80 character line limit):
#'   my_func <- function(args, causing, the, function, header, to, overflow, the, linewidth) {
#'   }
#'
#'   # Lint-free:
#'   my_func <- function(args, causing, the, function, header, to, overflow, the,
#'     linewidth) {
#'   }
#'
#'
#'   # Lint (a multi-line parameter default is mixed with spanning arguments):
#'   my_func <- function(one, multiline = {
#'       1L,
#'     }, arg) {
#'   }
#'
#'   # Lint-free:
#'   my_func <- function(
#'     one,
#'     multiline = {
#'       1L
#'     },
#'     args) {
#'   }
#'
#'
#' Checks function call arguments
#'
#'   # Lint (overflows 80 character line limit):
#'   vapply(mtcars, function(i) { i["mpg"] / i["wt"] }, numeric(1L), USE.NAMES = FALSE)
#'
#'   # Lint-free:
#'   vapply(mtcars, function(i) { i["mpg"] / i["wt"] }, numeric(1L),
#'     USE.NAMES = FALSE)
#'
#'
#'   # Lint (a multi-line argument is mixed with spanning arguments):
#'   lapply(data, function(i) {
#'       i
#'     })
#'
#'   # Lint-free:
#'   lapply(
#'     data,
#'     function(i) {
#'       i
#'     })
#'
#'
#' @importFrom xml2 xml_attr
#'
argument_wrapping_linter <- function(source_file, expr_xml_node, length = 80L,
  indent = {
    2L
  }) {

  indent_anchor_xml_node <- argument_indent_anchor(expr_xml_node)
  anchor_attrs <- xml2::xml_attrs(indent_anchor_xml_node)
  mode(anchor_attrs) <- "numeric"

  args_xml_nodeset <- xml2::xml_find_all(
    expr_xml_node,
    paste0(
      c("expr", "SYMBOL_FORMALS"),
      "[preceding-sibling::OP-LEFT-PAREN]",
      "[following-sibling::OP-RIGHT-PAREN]",
      collapse = "|"))

  args_attrs <- do.call(rbind, xml2::xml_attrs(args_xml_nodeset))
  mode(args_attrs) <- "numeric"

  arg_is_multi_line <- args_attrs[,"line1"] != args_attrs[,"line2"]
  arg_is_indented <- (args_attrs[,"col1"] - anchor_attrs["col1"]) == indent

  preceeding_line <- c(anchor_attrs["line1"], head(args_attrs[,"line2"], -1L))
  arg_line_diff <- args_attrs[,"line1"] - preceeding_line

  lint <- rep(list(NULL), 3L)

  # multiline arguments are mixed with multiple-args-per-line style
  if (any(arg_is_multi_line) && !all(arg_line_diff == 1)) {
    lint[[1]] <- Lint(
      filename = source_file$filename,
      line_number = args_attrs[1, "line1"],
      column_number = args_attrs[1, "col1"],
      type = "style",
      message = paste0(
        "if any argument must span multiple lines, then all arguments should ",
        "start on a new line."),
      line = "",
      ranges = NULL,
      linter = "argument_wrapping_linter")
  }

  # every argument in a one-arg-per-line style should be indented
  if (any(arg_is_multi_line && !all(arg_is_indented))) {
    lint[[2]] <- Lint(
      filename = source_file$filename,
      line_number = args_attrs[1, "line1"],
      column_number = args_attrs[1, "col1"],
      type = "style",
      message = paste0(
        "if any argument must span multiple lines, then all arguments should ",
        "be indented."),
      line = "",
      ranges = NULL,
      linter = "argument_wrapping_linter")
  }

  # if no multiline arguments and
  if (!any(arg_is_multi_line) && any(args_attrs[,"col2"] > length)) {
    first_arg_idx <- head(which(args_attrs[,"col2"] > length), 1L)
    lint[[3]] <- Lint(
      filename = source_file$filename,
      line_number = args_attrs[first_arg_idx, "line1"],
      column_number = args_attrs[first_arg_idx, "col1"],
      type = "style",
      message = sprintf(
        paste0(
          "if any argument overflows a %d character line limit, wrap and ",
          "indent additional arguments on a new line."),
        length),
      line = "",
      ranges = if (arg_is_multi_line[first_arg_idx]) {
          NULL
        } else {
          args_attrs[first_arg_idx, c("col1", "col2")]
        },
      linter = "argument_wrapping_linter")
  }

  Filter(Negate(is.null), lint)
}


argument_indent_anchor <- function(arg_expr_xml_node) {
  # root indentation is argument assignment by `=` operator
  indent_anchor_xml_node <- xml2::xml_find_all(
    arg_expr_xml_node,
    "./preceding-sibling::EQ_SUB[1]/preceding-sibling::*[1]")

  if (length(indent_anchor_xml_node))
    return(indent_anchor_xml_node[[1]])

  # if no indentation anchor found, use expression parent
  indent_anchor_xml_node <- xml2::xml_parent(arg_expr_xml_node)

  # if the parent is the rhs of assignment, anchor on the lhs

  indent_anchor_xml_node
}
