#' Recursion and stacks
#' =====
#' 
#' 
library(rstackdeque)
library(ggplot2)
library(reshape2)
#' ## How many licks does it take to get to a stack-overflow?
#' 
#' 1. How big of a recursive stack will R handle?
#' 
#' I'll tackle this problem by simply defining a neverending function that 
#' appends a call to a global stack and then calls itself again.
useless_recursion <- function(n){
  CALL_STACK <<- insert_top(CALL_STACK, n)
  useless_recursion(n + 1)
}

CALL_STACK <<- rstack()
try(useless_recursion(0))
length(CALL_STACK)
#' Interestingly, when I tried a method utilizing the CALL_COUNTER global, it 
#' turns out a little differently.
useless_recursion2 <- function(){
  CALL_COUNTER <<- CALL_COUNTER + 1
  CALL_STACK <<- insert_top(CALL_STACK, CALL_COUNTER)
  useless_recursion2()
}

CALL_STACK <<- rstack()
CALL_COUNTER <<- 0
try(useless_recursion2())
length(CALL_STACK)
#' I seem to get the value 2464 in the knitr document, but 2489 when I run it manually.
#' Let's see what happens when I utilize the `sumupto()` function from Exercise_one.
tfun <- function(n){
  CALL_COUNTER <<- CALL_COUNTER + 1
  if (n == 1){
    return(1)
  } else {
    tn <- n + tfun(n - 1)
    return(tn)
  }
}

CALL_COUNTER <<- 0
try(tfun(3000))
CALL_COUNTER
CALL_COUNTER <<- 0
try(tfun(824))
CALL_COUNTER
CALL_COUNTER <<- 0
try(tfun(823))
823*3
#' It seems that the stack for an R function is dependent on the function itself. I'm 
#' not exactly sure why...
useless_recursion3 <- function(n){
  if (n == 1){
    CALL_STACK <<- insert_top(CALL_STACK, n)
    return(n + useless_recursion3(n))
  } else if (n == 0){
    CALL_STACK <<- insert_top(CALL_STACK, "whee")
    return(useless_recursion3(n))
  } else { 
    CALL_STACK <<- insert_top(CALL_STACK,  n + n - 1)
    return(n + useless_recursion3(n) - useless_recursion3(n - 1))
  }
}
CALL_STACK <<- rstack()
try(useless_recursion3(9001))
length(CALL_STACK)
CALL_STACK <<- rstack()
try(useless_recursion3(1))
length(CALL_STACK)
CALL_STACK <<- rstack()
try(useless_recursion3(0))
length(CALL_STACK)
#' 
#' It almost seems as if the call stack is affected by any operations done on local 
#' variables.
#' 
#' ## Problem 2: Nicholson-Bailey Model
#' 
#' $$
#' H_{t} = \gamma \cdot H_{t-1} \cdot e^{-a \cdot P_{t-1}}\\
#' P_{t} = H_{t-1} \cdot \left(1 - e^{-a \cdot P_{t-1}}\right)
#' $$
#' 
#' Write a function for these assuming that $t_1$ has 10 hosts and 2 parasites.
#' Instead of doing 10 hosts and 2 parasites, I'm drawing randomly from a 
#' Poisson distribution with $\lambda = 2$ and $\lambda = 10$ for parasites and
#' hosts, respectively.  
hosts <- function(t, gamma = 2, a = 0.1, seed = 9999){
  if (t == 1){
    set.seed(seed)
    Ht1 <- rpois(1, 10)
    return(Ht1)
  } else {
    Htm1 <- hosts(t - 1, gamma, a, seed)
    Ptm1 <- parasites(t - 1, gamma, a, seed)
    Ht   <- gamma * Htm1 * exp(-a * Ptm1)
    return(Ht)
  }
}
parasites <- function(t, gamma = 2, a = 0.1, seed = 9999){
  if (t == 1){
    set.seed(seed)
    Pt1 <- rpois(1, 2)
    return(Pt1)
  } else {
    Htm1 <- hosts(t - 1, gamma, a, seed)
    Ptm1 <- parasites(t - 1, gamma, a, seed)
    return(Htm1 * (1 - exp(-a * Ptm1)))
  }
}
#' 
#' Now to run the simulations 10 times to see what happens. 
#+ cache = TRUE
HP_STACK <<- rstack()
set.seed(9999)
seeds <- sample(10000, 5)
for (r in 1:2){    
  seed <- seeds[r]
  for (i in 1:20){
    df <- data.frame(list(host     = hosts(i, seed = seed),
                          parasite = parasites(i, seed = seed),
                          t        = i,
                          seed     = seed
                          )
                     )
    set.seed(seed); Nhost <- rpois(1, 10)
    set.seed(seed); Npar  <- rpois(1, 2)
    df$seed <- paste("Seed =", seed, ", H:P =", paste(Nhost, Npar, sep = ":"))
    HP_STACK <- insert_top(HP_STACK, df)
  }
}
hpdf <- as.data.frame(HP_STACK)
#' Now to plot
ggplot(melt(hpdf, measure.vars = c("host", "parasite")), 
       aes(x = t, y = value, color = variable)) + 
  geom_line() + facet_wrap(~seed, scales = "free_y") + theme_classic()
