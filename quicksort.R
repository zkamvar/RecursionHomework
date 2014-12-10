quicksort <- function(elements){
  le <- length(elements)
  if (le <= 1){
    return(elements)
  }
#   pivot <- sample(elements, 1)
  pivot       <- median(elements)            # Work: n (if using linear time median)
  lesslist    <- elements[elements < pivot]  # Work: n
  equallist   <- elements[elements == pivot] # Work: n
  greaterlist <- elements[elements > pivot]  # Work: n
  
  less_sorted    <- quicksort(lesslist)
  greater_sorted <- quicksort(greaterlist)
  return(c(less_sorted, equallist, greater_sorted)) # Work: n
}

samp <- as.integer(runif(10000, 1, 1000))
print(head(samp))
sorted <- quicksort(samp)
print(head(sorted))


