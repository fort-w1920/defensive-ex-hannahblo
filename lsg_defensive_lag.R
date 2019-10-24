# Lösung d1

library(testthat)
library(checkmate)

lag <- function(x, n = 1L) {
  
  # testing if n is a natural number
  assert(check_int(n), check_integer(n), check_count(n))
  
  # converting list- to vector-object
  if (test_list(x)) {
    x <- unlist(x)
    warning("converting ", x, " from list to vector object" )
  }
  
  # testing if x vector object
  assert_vector(x, strict = TRUE)
  
  # testing if n and length of x are incompatibel with seq_lag
  assert_count(length(x) - n)
  
  # calculating return
  xlen <- length(x)
  c( rep(NA, n), x[seq_len(xlen - n)] )
}


# Defining testfunctions
expect_error(lag(c(5), 1.1))
expect_error(lag( data.frame(a = c(1,2), b = c(3,4)), 1 ))
expect_error( lag( matrix( c(2, 4, 3, 1, 5, 7), nrow = 3, ncol = 2 ), 1L))
expect_error(lag(c(1,2), 1.1))
expect_error( lag(c(1,2), 4))
expect_error(lag(c(a,b), 1))

expect_warning( (lag(list(1,2,3), 2)) )

# as.logical is needed since class(lag(c(1,2), 2L)) == numeric and 
#       ca(NA,NA) == logical
expect_identical(as.logical(lag(c(1,2), 2L)), c(NA, NA))
expect_identical(lag(c(1,2,3), 1), c(NA, 1,2))

