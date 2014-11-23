#' Exercise 3: third time's to harm
#' ======
#' 
#' This set of exercises will build off of exercise two, which was built off of
#' exercise one. The purpose of this will be to utilize memoization and dynamic
#' programming as alternative ways of solving the Nicholson-Bailey Model
#' 
#' ## Necessary packages
library(hash)
library(stringr)
library(microbenchmark)
library(reshape2)
library(ggplot2)
#' 
#' ## The Nicholson-Bailey Model
#' 
#' ### Formulaic definition
#' $$
#' H_{t} = \gamma \cdot H_{t-1} \cdot e^{-a \cdot P_{t-1}}\\
#' P_{t} = H_{t-1} \cdot \left(1 - e^{-a \cdot P_{t-1}}\right)
#' $$
#' 
#' ### Implementation in Exercise two
#' 
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
#' ## Implemenentation one: memoization
#' 
#' The point of memoization is to utilize global hashes tables in order to avoid
#' redundancy in compuation. These hash tables are utilized within the recursive 
#' function calls to determine if the value has already been computed. Here, we
#' are setting up the tables.
#' 
HOST_HASH <<- hash()
PARASITE_HASH <<- hash()
#'
#' I am going to add a couple of extra parameters into the functions, I will add
#' the variables `np` and `nh`, standing for "number of parasites" and "number
#' of hosts", repsectively. These will allow me to explicitly set the parameters
#' for the starting number of hosts and parasites OR the $\lambda$ value if the
#' seed is set.
make_the_call <- function(t, gamma, a){
  return(str_c(as.character(t), ":", as.character(gamma), ":", as.character(a)))
}

hosts_mem <- function(t, gamma = 2, a = 0.1, seed = 9999, nh = 10, np = 2){
  thiscall <- make_the_call(t, gamma, a)
  if (has.key(thiscall, HOST_HASH)){
    return(HOST_HASH[[thiscall]])
  }
  if (t == 1){
    if (is.null(seed)){
      Ht1 <- nh
    } else {
      set.seed(seed)
      Ht1 <- rpois(1, nh)      
    }
    HOST_HASH[[thiscall]] <<- Ht1
    return(Ht1)
  } else {
    Htm1 <- hosts_mem(t - 1, gamma, a, seed, nh, np)
    Ptm1 <- parasites_mem(t - 1, gamma, a, seed, nh, np)
    Ht   <- gamma * Htm1 * exp(-a * Ptm1)
    HOST_HASH[[thiscall]] <<- Ht
    return(Ht)
  }
}
parasites_mem <- function(t, gamma = 2, a = 0.1, seed = 9999, nh = 10, np = 2){
  thiscall <- make_the_call(t, gamma, a)
  if (has.key(thiscall, PARASITE_HASH)){
    return(PARASITE_HASH[[thiscall]])
  }
  if (t == 1){
    if (is.null(seed)){
      Pt1 <- np
    } else {
      set.seed(seed)
      Pt1 <- rpois(1, np)
    }
    PARASITE_HASH[[thiscall]] <<- Pt1
    return(Pt1)
  } else {
    Htm1 <- hosts_mem(t - 1, gamma, a, seed, nh, np)
    Ptm1 <- parasites_mem(t - 1, gamma, a, seed, nh, np)
    Pt   <- Htm1 * (1 - exp(-a * Ptm1))
    PARASITE_HASH[[thiscall]] <<- Pt
    return(Pt)
  }
}

#' ### Testing memoization v. non-memoization 
#+ cache = TRUE
bench_host <- function(...){
  HOST_HASH <<- hash()
  PARASITE_HASH <<- hash()
  hosts_mem(...)
}
bench_parasite <- function(...){
  HOST_HASH <<- hash()
  PARASITE_HASH <<- hash()
  parasites_mem(...)
}
(benchmarks <- microbenchmark(hosts(20),  parasites(20), bench_host(20), 
                             bench_parasite(20), times = 1L))
#' 
#' That's a hell of an improvement!
#'
#'
#' ## Implementation two: dynamic programming
#' 
#' Since the base case is known, we can simply construct a table of values and
#' start from the bottom up. 
host_parasite <- function(t, gamma = 2, a = 0.1, seed = 9999, nh = 10, np = 2){
  hp_matrix <- matrix(nrow = 2, ncol = t, 
                      dimnames = list(entity = c("host", "parasite"),
                                      time   = seq(1, t)
                                      )
                      )
  
  if (is.null(seed)){
    hp_matrix["host", 1]     <- nh
    hp_matrix["parasite", 1] <- np
  } else {
    set.seed(seed)
    hp_matrix["host", 1]     <- rpois(1, nh)
    set.seed(seed)
    hp_matrix["parasite", 1] <- rpois(1, np)
  }

  for (i in seq(2, t)){
    Pt <- hp_matrix["host", i - 1] * (1 - exp(-a * hp_matrix["parasite", i - 1]))
    Ht <- gamma * hp_matrix["host", i - 1] * exp(-a * hp_matrix["parasite", i - 1])
    hp_matrix["parasite", i] <- Pt
    hp_matrix["host", i]     <- Ht
  }
  return(hp_matrix)
}
#'
#' ### Now to test to see if it works:
#' 
host_parasite(20)
#' 
#' it does indeed. Let's test it against the hashing methods. Since these are not long
#' running, I will increase the number of replications.
#+ cache = TRUE
benchmarks2 <- microbenchmark(host_parasite(20), bench_host(20), 
                              bench_parasite(20), host_parasite(40), 
                              bench_host(40), bench_parasite(40),
                              times = 1000L)
benchmarks2
autoplot(benchmarks2)
#' 
#' 
#' Now, I will explore some scenarios with some random seeds. Note that there
#' end up being duplicate entries in our table, so there won't be 100 plots.
#'  
#+ fig.width = 10, fig.height = 7
set.seed(9001)
seeds <- sample(10000, 100)
hp_array <- array(dim = c(2, 20, length(seeds)), 
                  dimnames = list(entity = c("host", "parasite"),
                                  time   = 1:20,
                                  seed   = NULL)
                  )
for (i in 1:length(seeds)){
  hp_array[, , i] <- host_parasite(20, seed = seeds[i])
}
dimnames(hp_array)$seed <- apply(hp_array, 3, function(x) paste0("H:P = ", x[1, 1], ":", x[2, 1]))

ggplot(melt(hp_array), aes(x = time, y = value, color = entity)) + geom_line() + 
  facet_wrap(~seed, scales = "free_y") + theme_classic()