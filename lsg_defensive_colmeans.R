# Lösung d3

library(checkmate)

col_means <- function(input, na.rm = FALSE) {
  
  # checking input
  assert(check_data_frame(input), check_numeric(input), check_matrix(input),
         check_list(input)) 

  df <- as.data.frame(input)
  
  # test if input is empty, if it's empty: throw warning and return empty 
  # dataframe
  if (!test_data_frame(df, min.rows = 1) || 
      !test_data_frame(df, min.cols = 1)) {
    warning("input is empty")
    return(data.frame())
  }
  
  # selecting columns which values are numeric 
  numeric <- vapply(df, is.numeric, logical(1))
  names_col <- names(numeric[which(numeric == TRUE)])
  numeric_cols <- as.data.frame(df[, numeric])
  
  # test if numeric columns exist, if not: throw warning and return empty 
  # dataframe
  if (!test_data_frame(numeric_cols, min.cols = 1)) {
    warning("input has no values wich is convertable to numeric")
    return(data.frame())
  }
  
  # calculating mean
  mean_col <- data.frame(lapply(numeric_cols, mean, na.rm = na.rm))
  names(mean_col) <- names_col
  
  return(mean_col)
}
