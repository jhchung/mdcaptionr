library(mdcaptionr)
library(testthat)

test_that("marker_obj intializes a new marker object", {
  test_markers <- marker_obj()
  expect_is(test_markers, "marker_obj")
})

test_that("extract_marker_type extracts marker", {
  expect_match(extract_marker_type("fig.foo"), "^fig$")
  expect_error(extract_marker_type("fig.foo.bar"))
  expect_error(extract_marker_type("foo"))
})

test_that("label adds new marker",{
  test_markers <- marker_obj()
  test_markers <- label(test_markers, "fig.foo")
  expect_equal(length(test_markers), 1)
  expect_equal(length(test_markers$fig.foo), 3)
  expect_match(test_markers$fig.foo$type, "^fig$")
  expect_warning(label(test_markers, "fig.foo"))
})

test_that("label adds multiple markers", {
  test_markers <- marker_obj()
  test_markers <- label(test_markers, "fig.foo")
  test_markers <- label(test_markers, "fig.bar")
  expect_equal(length(test_markers), 2)
  expect_equal(test_markers$fig.bar$label, 2)
  expect_warning(label(test_markers, "fig.bar"))

  test_markers <- label(test_markers, "tab.foo")
  expect_equal(length(test_markers), 3)
  expect_equal(test_markers$tab.foo$label, 1)
})

test_that("ref gets correct label", {
  test_markers <- marker_obj()
  test_markers <- label(test_markers, "fig.foo")
  test_markers <- label(test_markers, "fig.bar")
  test_markers <- label(test_markers, "tab.foo")
  expect_equal(ref(test_markers, "fig.foo"), 1)
  expect_equal(ref(test_markers, "fig.bar"), 2)
  expect_equal(ref(test_markers, "tab.foo"), 1)
  expect_warning(ref(test_markers, "foo"))
})

test_that("check_if_prefix_is_supplemental", {
  test_label_1 <- "supplemental figure"
  test_label_2 <- "Supplemental Figure"
  test_label_3 <- "Figure S"
  test_label_4 <- "Figure s"
  test_label_5 <- "Figure"
  expect_true(check_if_prefix_is_supplemental(test_label_1))
  expect_true(check_if_prefix_is_supplemental(test_label_2))
  expect_true(check_if_prefix_is_supplemental(test_label_3))
  expect_true(check_if_prefix_is_supplemental(test_label_4))
  expect_false(check_if_prefix_is_supplemental(test_label_5))
})

test_that("print_label works", {
  test_markers <- marker_obj()
  test_markers <- label(test_markers, "fig.foo")
  expect_match(print_label(test_markers, "fig.foo", "Figure"), "Figure 1:")
  expect_match(print_label(test_markers, "fig.foo", "Figure S"), "Figure S1:")
  expect_match(print_label(test_markers, "fig.foo", "Supplemental Figure"),
               "Supplemental Figure 1:")
  expect_match(print_label(test_markers, "fig.foo", "Figures"), "Figures 1:")
})
