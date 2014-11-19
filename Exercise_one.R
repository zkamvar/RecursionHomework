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
#' 
#' ### Question: what's the relationship between `fib(n)` and `CALL_COUNTER`?
#' 
#' First assumption: we know that `CALL_COUNTER` is always greater than or equal
#' to `fib(n)`
all(restab[[2]] <= CALL_COUNTER)
#' 
#' Now that we've established that, we should try to find some relationship
#' between the two. One easy way to start is to plot `fib(n)` against
#' `CALL_COUNTER` and see if there is a linear relationship.
plot(restab[[2]], restab[[3]])
#' 
#' There is an apparent linear relationship Let's look at the differences. 
(call_diff <- restab[[3]] - restab[[2]])
#' 
#' and the differences between the differences
diff(call_diff)
#' The differences between the differences result in a fibonacci sequence. This 
#' makes sense given the linear relationship. If we think in terms of what the 
#' function is doing, in the simplest two cases, the function is being called 
#' once. In the case of 3, the function is called 3 times:
#' 
#' ```
#' 2 = fib(3) -> fib(2) + fib(1)
#' ```
#' 
#' In the case of 4, the function is called 5 times:
#' 
#' ```
#' 3 = fib(4) -> (fib(3) -> fib(2) + fib(1)) + 
#'               fib(2)
#' ```
#' 
#' In the case of 5, the function is called 9 times:
#' 
#' ```
#' 5 = fib(5) -> (fib(4) -> (fib(3) -> fib(2) + fib(1)) + fib(2)) + 
#'               (fib(3) -> fib(2) + fib(1))
#' ```
#' 
#' What happens is that the function is called twice within the function if the 
#' case is not 2 or 1. So, we can see whether or not multiplying the call by two
#' will work. The difference between these numbers should be zero if this works.
restab[[2]]*2 - restab[[3]]
#' 
#' That didn't work, but we have all ones. This means that the linear
#' relationship is indeed double, but there's an extra call left over. If we
#' think about it, we actually have three calls per call, but since there is the
#' initial call that's not taken into account, we need to subtract it from the
#' doubling effort. This will bring it to zero.
#' 
#' How can we prove this? One way to do this would be to predict the next 
#' CALL_COUNTER value by utilizing the next number in the fibonacci sequence.
(nextfib  <- restab[[2]][19] + restab[[2]][20])
(nextcall <- nextfib*2 - 1)
CALL_COUNTER <- 0
fib(21)
CALL_COUNTER
CALL_COUNTER == nextcall
#' 
#' The formula to calculate the number of calls needed to calculate the 
#' $n^{th}$ fibonacci number:
#' $\left(2 \times fib(n)\right) - 1$
#' 
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
