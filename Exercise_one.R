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
#' ## Packages and global variables
library(knitr)
library(ggplot2)
library(reshape2)
CALL_COUNTER <- 0
#' 
#' 
#' ## Fibonacci function
#' ### Definition
fib <- function(n, call_count = TRUE){
  if (call_count){
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
#' 
#' ### Initial tests
#' 
#' Now that we have the function, we should run it to make sure everything works
fib(1)
fib(2)
fib(8)
fib(10)
#'
#' ### Creating table
#' 
#' Excellent, now let's see what happens from 1 to 20. Here I am setting a
#' maximum number, creating a data frame to hold the results, and making a for
#' loop to loop through all the numbers. 
n <- 20
restab <- data.frame(list(n = 1:n, 
                          `Fibonacci number` = 1:n, 
                          `Number of Calls` = 1:n),
                     check.names = FALSE)
for (i in 1:n){
  CALL_COUNTER <- 0
  fib_res      <- fib(i)
  restab[i, ]  <- c(i, fib_res, CALL_COUNTER)
}

#' ### Pretty things
#' 
#' Now I'm using kable from the package knitr to make the results pretty and I'm
#' plotting with ggplot2 and reshape2
knitr::kable(restab)
meltres <- reshape2::melt(restab, measure.vars = 2:3)
ggplot(meltres, aes(x = n, y = value, color = variable)) + geom_line()
#' 
#' ## Factorials!
#' 
#' A factorial is simply the product of n by all the integers that come before it
#' which are nonzero. 
#' 
#' ### Definition
fact <- function(n, call_count = TRUE){
  if (call_count){
    CALL_COUNTER <<- CALL_COUNTER + 1
  }
  if (n == 1){
    return(1)
  } else {
    fn <- n * fact(n - 1)
    return(fn)
  }
}
#' 
#' ### Intial tests
fact(1) == 1
fact(2) == 2*1
fact(5) == 5*4*3*2*1
#' 
#' ### Table Time
n <- 20
names(restab)[2] <- "Factorial"
for (i in 1:n){
  CALL_COUNTER <- 0
  fact_res      <- fact(i)
  restab[i, ]  <- c(i, fact_res, CALL_COUNTER)
}
#' ### Pretty things
#' 
knitr::kable(restab)
meltres <- reshape2::melt(restab, measure.vars = 2:3)
ggplot(meltres, aes(x = n, y = value, color = variable)) + geom_line()
#'
#'
#' ## Terminal function
#' 
#' This one is like the factorial, but it adds numbers instead of multiplies 
#' them. This is like taking the last value of a cumulative sum from 1 to n.
#' 
tfun <- function(n, call_count = TRUE){
  if (call_count){
    CALL_COUNTER <<- CALL_COUNTER + 1
  }
  if (n == 1){
    return(1)
  } else {
    tn <- n + tfun(n - 1)
    return(tn)
  }
}
#' ### Intial tests
tfun(1) == 1
tfun(2) == cumsum(1:2)[2]
tfun(5) == cumsum(1:5)[5]
#' 
#' ### Table Time
n <- 20
names(restab)[2] <- "$n^{th}$ triangular number"
for (i in 1:n){
  CALL_COUNTER <- 0
  tfun_res      <- tfun(i)
  restab[i, ]  <- c(i, tfun_res, CALL_COUNTER)
}
#' ### Pretty things
#' 
knitr::kable(restab)
meltres <- reshape2::melt(restab, measure.vars = 2:3)
ggplot(meltres, aes(x = n, y = value, color = variable)) + geom_line()
#'
