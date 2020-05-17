#'
#' @describeIn linters checks that function arguments wrap with a trailing
#'   indent with arguments spanning an 80 character limit or one argument per
#'   line.
#'
#' @importFrom xml2 xml_find_all as_xml_document xml_attr
#' @export
#'
function_call_argument_wrapping_linter <- function(source_file) {
  argument_wrapping_linter(
    source_file,
    xml2::xml_find_all(
      source_file$xml_parsed_content,
      paste0(
        "//expr",
        "//SYMBOL_FUNCTION_CALL",
        "/..",
        "/parent::*")))
}



#'
#' @describeIn linters checks that function parameters wrap with a preceding
#'   indent when they overflow the line width, or that all parameters are
#'   on provided on their own line if any parameters are multi-line expressions.
#'
#' @importFrom xml2 xml_find_all as_xml_document
#' @export
#'
function_header_argument_wrapping_linter <- function(source_file) {
  argument_wrapping_linter(
    source_file,
    xml2::xml_find_all(
      source_file$xml_parsed_content,
      "//FUNCTION/parent::*"))
}




#' Lints argument formatting styles
#'
#' Argument formatting can come in a number of different forms. To wrangle
#' this styling, a couple linters are introduced to impose some structure to
#' argument formatting - especially for lengthy expressions commonly including
#' anonymous functions or inline expressions.
#'
#' \itemize{
#'   \item{Function arguments are wrapped to a new line if they span beyond a
#'   specified line limit.}
#'   \item{If any single argument is a multi-line expression, all arguments
#'   should be introduced on new lines.}
#'   \item{Any argument introduced on a new line should be indented relative to
#'   the indentation of the expression returned by \code{indent_anchor}.}
#' }
#'
#' @param source_file An expression returned from \code{get_source_expressions}
#' @param expr_xml_nodeset A \code{xml_nodeset} specific to a function header or
#'   call expression.
#' @param length Additional arguments unused
#'
#' @importFrom xml2 xml_attr
#'
argument_wrapping_linter <- function(source_file, expr_xml_nodeset, ...) {
  # short-circuit on global expression
  if (!is.null(source_file$full_parsed_content))
    return(list())

  flatten_lints(lapply(
    expr_xml_nodeset,
    function(expr_xml_node) {
      call_sym_attrs <- numeric_xml_attrs(expr_xml_node)

      args_xml_nodeset <- xml2::xml_find_all(
        expr_xml_node,
        paste0(
          c("expr", "SYMBOL_FORMALS"),
          "[preceding-sibling::OP-LEFT-PAREN]",
          "[following-sibling::OP-RIGHT-PAREN]",
          collapse = "|"))

      if (!length(args_xml_nodeset)) return(list())

      args_attrs <- do.call(rbind, xml2::xml_attrs(args_xml_nodeset))
      mode(args_attrs) <- "numeric"

      arg_is_multi_line <- args_attrs[, "line1"] != args_attrs[, "line2"]
      prev_line <- c(call_sym_attrs["line1"], head(args_attrs[, "line2"], -1L))
      arg_line_diff <- args_attrs[, "line1"] - prev_line

      lint <- rep(list(NULL), 3L)

      # redefining some logicals for clarity
      only_last_arg_multi_line <- all(which(arg_is_multi_line) == length(arg_is_multi_line))
      arg_on_newline <- arg_line_diff == 1L

      # multiline arguments are mixed with multiple-args-per-line style
      # except:
      #   situation where only the last arg spans multiple lines and all
      #   other args are on the same line
      if ((any(arg_is_multi_line) && !all(arg_on_newline)) &&
        !(only_last_arg_multi_line && !any(arg_on_newline))) {

        lint[[1]] <- Lint(
          filename = source_file$filename,
          line_number = args_attrs[1, "line1"],
          column_number = args_attrs[1, "col1"],
          type = "style",
          message = paste0(
            "All arguments should start on the next new line if any arguments ",
            "wrap, or any argument except the last spans multiple lines."),
          line = "",
          ranges = NULL,
          linter = "argument_wrapping_linter")
      }

      Filter(Negate(is.null), lint)
    }))
}



#' Retrieve a parent expression to anchor indentation
#'
#' @param expr_xml_node An xml node, relative to which a parent expression will
#'   be found as an anchor for indentation
#'
#' @export
#'
get_indent_anchor_attrs <- function(expr_xml_node, indent = 2L) {
  # root indentation is `=` assignment expression
  indent_anchor_xml_node <- xml2::xml_find_first(
    expr_xml_node,
    "./preceding-sibling::EQ_SUB[1]/..")

  # root indentation is `<-` assignment expression
  if (!length(indent_anchor_xml_node))
    indent_anchor_xml_node <- xml2::xml_find_first(
      expr_xml_node,
      "./preceding-sibling::LEFT_ASSIGN[1]/..")

  if (!length(indent_anchor_xml_node)) {
    # expr node root has type "expr", return node itself
    if (xml2::xml_name(expr_xml_node) == "expr") {
      indent_anchor_xml_node <- expr_xml_node
    # if no indentation anchor found, use expr parent
    } else {
      indent_anchor_xml_node <- xml2::xml_parent(expr_xml_node)
    }
  }

  expr_attrs <- numeric_xml_attrs(expr_xml_node)
  attrs <- numeric_xml_attrs(indent_anchor_xml_node)

  # if the function call spans to a new line in the parent expression, add
  # a hanging indent... perhaps this should be moved to a separate linter
  # to impose wrapping of run-on single line expressions?
  if (attrs["line1"] != expr_attrs["line1"])
    indent <- indent * 2L

  attrs[c("col1", "col2")] <- attrs[c("col1", "col2")] + indent
  attrs
}



# Retrieve node attributes and convert to numeric
numeric_xml_attrs <- function(xml_node) {
  attrs <- xml2::xml_attrs(xml_node)
  mode(attrs) <- "numeric"

  attrs
}
