test_that("example policies rate as expected", {
  ex <- example_rating_plan()
  rated <- rate_policies(ex$policies, ex$plan)

  expect_equal(rated$indicated_BI, c(111, 134, 170))
  expect_equal(rated$indicated_PD, c(92, 111, 132))
})

test_that("trace final values match rated values", {
  ex <- example_rating_plan()
  result <- rate_policies_with_trace(ex$policies, ex$plan)
  rated <- result$rated_data
  trace <- result$term_trace

  for (i in seq_len(nrow(rated))) {
    for (cov in c("BI", "PD")) {
      rows <- trace[trace$row_id == i & trace$coverage == cov, , drop = FALSE]
      expect_equal(tail(rows$value_after_step, 1), rated[[paste0("indicated_", cov)]][[i]])
    }
  }
})
