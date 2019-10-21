dataset <- "DUR_D"

test_that("get_data_structure returns a list of data frame with positive length", {
  skip_on_cran()
  Sys.sleep(10)
  dstruc <- get_data_structure(dataset)

  expect_equal(class(dstruc), "list")
  expect_true(length(dstruc) > 0)
  expect_true(all(lapply(dstruc, class) == "data.frame"))
})

test_that("get_dataset returns a data frame with positive length", {
  skip_on_cran()
  Sys.sleep(10)
  filter_list <- list(c("DEU", "FRA"), "MW", "2024")
  df <- get_dataset(dataset = dataset, filter = filter_list)
  
  expect_equal(class(df), c("tbl_df", "tbl", "data.frame"))
  expect_true(nrow(df) > 0)
})
