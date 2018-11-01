context("tableHTML_to_image testing")

test_that("tableHTML is successfully saved as PNG", {
 file = tempfile(pattern = "output",
                 fileext = ".png")

 # skip_on_cran()
 expect_false(file.exists(file))

 mtcars %>%
  tableHTML() %>%
  tableHTML_to_image(file = file)

 expect_true(file.exists(file))

 expect_gt(file.info(file)$size, 0)
})

test_that("tableHTML is successfully saved as JPEG", {
 file = tempfile(pattern = "output",
                 fileext = ".jpeg")
 # skip_on_cran()

 expect_false(file.exists(file))

 mtcars %>%
  tableHTML() %>%
  add_css_table(css = list('background-color', 'white')) %>%
  tableHTML_to_image(file = file)

 expect_true(file.exists(file))

 expect_gt(file.info(file)$size, 0)
})

test_that("tableHTML is successfully saved as PDF", {
 file = tempfile(pattern = "output",
                 fileext = ".pdf")
 # skip_on_cran()

 expect_false(file.exists(file))

 mtcars %>%
  tableHTML() %>%
  tableHTML_to_image(file = file)

 expect_true(file.exists(file))

 expect_gt(file.info(file)$size, 0)
})
