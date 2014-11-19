#' Stacks! (*and* hashes?) 
#' ========
#' 
#' ## Stacks
#' 
#' Imagine a stack like a stack of plates/pancakes.
#' 
#' FILO: First In Last Out
#' 
#' Operations: 
#' 
#' - PUSH: Add thing to top of stack 
#' - POP: Remove thing from top of
#' stack 
#' - PEEK: Look at thing from top of stack, but don't remove.
#' 
#' All these operations are fast independent of the size of the stack, constant
#' time $O(1)$. (in python, stack is unsorted linked list).
#' 
#' ## Queues
#' 
#' FIFO: First In First Out 
#' 
#' (deque "deck" == double ended queue)
#' 
#' # Examples
library(rstackdeque)
#' ### Basic Rstack insertions
s <- rstack()
s <- insert_top(s, "A")
s <- insert_top(s, "B")
s <- insert_top(s, "C")
print(s)
print(peek_top(s))
#' ### Basic Rstack pops
s <- without_top(s)
print(s)
g <- insert_top(s, "X")
print(s)
print(g)
glist <- as.list(g)
print(glist)
#' ### inserting data frame rows fast
rows <- rstack()
rows <- insert_top(rows, list(name = "bob", age = 24))
rows <- insert_top(rows, list(name = "joe", age = 27))
print(rows)
rowsdf <- as.data.frame(rows)
print(rowsdf)
#' ## Introducing coolness
#' 
#' Remember the fibonacci sequence function from Exercise_one.R
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
#' Now we are going to produce a table from before with stacks.
library(rstackdeque)
info <- rstack()
for (i in seq(1, 15)){
  CALL_COUNTER <- 0
  row  <- data.frame(list(n = i, fibn = fib(i), calls = CALL_COUNTER))
  info <- insert_top(info, row)
}
library(ggplot2)
infodf <- as.data.frame(info)
p <- ggplot(infodf) + 
  geom_line(aes(x = n, y = fibn)) + 
  geom_line(aes(x = n, y = calls))
try(plot(p))
#'
#' -----------------------------------
#' 
#' ## Local variables and stacks
#' 
#' ### Call stack
#'
#' Every time a function is called, info is put on this call stack.
#' When it returns, the call stack pops. 
#' 
#' In class: examine call stack of fibonacci sequence. 
#' 
#' We'll make our own call stack that traces the information of the fib function.
library(stringr)

print_string_stack <- function(s){
  char_vec <- as.character(as.list(s))
  message(rev(paste(char_vec, collapse = " ")))
}

fib <- function(n, call_count = TRUE){
  CALL_COUNTER <<- CALL_COUNTER + 1
  thiscall <- str_c("fib:n=", as.character(n))
  CALL_STACK <<- insert_top(CALL_STACK, thiscall)
  print_string_stack(CALL_STACK)
  if (n == 1 || n == 2){
    CALL_STACK <<- without_top(CALL_STACK)
    print_string_stack(CALL_STACK)
    return(1)
  } else {
    a <- fib(n - 1)
    b <- fib(n - 2)
    c <- a + b
    CALL_STACK <<- without_top(CALL_STACK)
    print_string_stack(CALL_STACK)
    return(c)
  }
}

CALL_STACK <<- rstack()
CALL_COUNTER <<- 0

info <- rstack()
fib(4)
#' 
#' 
#' $fib(n) = 1.614*fib(n-1)$