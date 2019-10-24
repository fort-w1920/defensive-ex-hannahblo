# Lösung d2


library(testthat)

count_them <- function(supposedly_a_count) {
  
  # assumptions for supposedly_a_count:
  #     it's an numeric, double value
  #     it's a scalar, no matric, dataframe etc.
  #     return value is an integer
  
  assert_numeric(supposedly_a_count, any.missing = FALSE,
                 finite = TRUE, len = 1, lower = 0)
  
  if (!checkmate::test_count(supposedly_a_count)) {
    warning(
      "rounding ", supposedly_a_count,
      " to the nearest integer."
    )
    supposedly_a_count <- round(supposedly_a_count)
  }
  
  
  if (!checkmate::test_integer(supposedly_a_count)) {
    warning(
      "converting ", supposedly_a_count, " to integer."
    )
    supposedly_a_count <- as.integer(supposedly_a_count)
  }
}
  



