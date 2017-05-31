context("Can we build the book?")

build_out <- rmarkdown::render_site("../../inst", output_format = 'bookdown::gitbook', encoding = 'UTF-8')

build_out_true <- grepl("index.html", build_out)

test_that("We can build the book", {
  expect_equal(build_out_true, TRUE)
})
