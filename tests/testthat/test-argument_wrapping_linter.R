context("func_header_argument_wrapping_linter")



test_that("lint suggests wrapping args when they span beyond line limit.", {
  expect_lint(
    paste(sep = "\n",
      "function(this, is, too, wide, should, be, wrapped) {",
      "}"),
    "line length",
    linters = list(function_header_argument_wrapping_linter(length = 40L)))

  expect_lint(
    paste(sep = "\n",
      "function(this, is, too, wide, should, be, wrapped) {",
      "}"),
    NULL,
    linters = list(function_header_argument_wrapping_linter(length = 60L)))

  expect_lint(
    paste(sep = "\n",
      "x <- function(this, is, too, wide, should, be, wrapped) {",
      "}"),
    "line length",
    linters = list(function_header_argument_wrapping_linter(length = 40L)))

  expect_lint(
    paste(sep = "\n",
      "x <- function(this, is, too, wide, should, be, wrapped) {",
      "}"),
    NULL,
    linters = list(function_header_argument_wrapping_linter(length = 60L)))

  expect_lint(
    paste(sep = "\n",
      "really_long_variable_that_requires_pushing_function_to_next_line <- ",
      "  function(this, is, too, wide, should,",
      "    be, wrapped) {",
      "  }"),
    NULL,
    linters = list(function_header_argument_wrapping_linter(length = 40L)))

  expect_lint(
    paste(sep = "\n",
      "x <- function(this, is, too, wide,",
      "              should, be, wrapped) {",
      "}"),
    NULL,
    linters = list(function_header_argument_wrapping_linter(
      length = 40L,
      indent_anchor = get_paren_indent_anchor_attrs)))
})



test_that("lint catches improper wrapped argument indentation.", {
  expect_lint(
    paste(sep = "\n",
      "function(this, is, too, wide, should, be, ",
      "    wrapped) {",
      "}"),
    "indent",
    linters = list(function_header_argument_wrapping_linter(indent = 2L)))

  expect_lint(
    paste(sep = "\n",
      "function(this, is, too, wide, should, be, ",
      "  wrapped) {",
      "}"),
    NULL,
    linters = list(function_header_argument_wrapping_linter(indent = 2L)))

  expect_lint(
    paste(sep = "\n",
      "x <- function(this, is, too, wide, should, be, ",
      "    wrapped) {",
      "}"),
    "indent",
    linters = list(function_header_argument_wrapping_linter(indent = 2L)))

  expect_lint(
    paste(sep = "\n",
      "x <- function(this, is, too, wide, should, be, ",
      "  wrapped) {",
      "}"),
    NULL,
    linters = list(function_header_argument_wrapping_linter(indent = 2L)))
})
