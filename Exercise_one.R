#' # Exercise One: Basic Recursion
#' 
#' There are three tasks in this exercise:
#' 
#' 1. Construct the fibonacci algorithm and make a table with N, the fibonacci
#' number, and the result of CALL_COUNTER. 
#' 2. Construct a factorial algorithm and create the same table. 
#' 3. Construct a terminal function to calculate the
#' $n^{th}$ triangular number.
#' 
#' ## Packages
library(knitr)
#' 
#' 
#' ## Fibonacci function
CALL_COUNTER <- 0

fib <- function(n, count = TRUE){
  if (count){
    CALL_COUNTER <<- CALL_COUNTER + 1
  }
  if (n == 1 || n == 2){
    return(1)
  } else {
    a <- fib(n - 1)
    b <- fib(n - 2)
    c <- a + b
    return(c)
  }
}

n <- 10
restab <- data.frame(list(n = 1:n, fib = 1:n, CALL_COUNT = 1:n))
for (i in 1:n){
  CALL_COUNTER <- 0
  restab$fib[i] <- fib(i)
  restab$n[i] <- i
  restab$CALL_COUNT[i] <- CALL_COUNTER
}

knitr::kabel(restab)