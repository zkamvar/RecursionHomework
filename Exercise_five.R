#' Global Alignment
#' ================
#' 
#' ### Packages
#' 
#' Loading the required packages as well as all of the functions from [Exercise
#' four](Exercise_four.html).
library(stringr)
library(rstackdeque)
library(hash)
source("Exercise_four.R")
devtools::session_info()
ls()
#' 
#' ### Recursive solution
#' First, we will present a recursive solution to global alignment. 
#' 
#' The idea is that if we have two sequences that we want to align that aren't
#' base cases, they will need to be split up into smaller problems in order to
#' solve them. 
#' 
#' Remember that to produce the base cases, we chop off the ends of the sequences
#' and call them $e_x$ and $e_y$. The prefixes become $P_x$ and $P_y$. Once we
#' do this, we have three cases:
#' 
#' - $^{(A)}P_x$ with $P_y$ and $e_x$ with $e_y$ 
#' - $^{(B)}P_x + e_x$ with $P_y$ and $-$ with $e_y$ 
#' - $^{(C)}P_x$ with $P_y + e_y$ and $e_x$ with $-$ 
#'
#' These alignments will produce the following scores:
#' 
#' - $score_A = S(A) + S(e_x + e_y)$
#' - $score_B = S(B) + S(- + e_y)$
#' - $score_C = S(C) + S(e_x + -)$
#' 
#' The best scores win!
global_aln <- function(x, y){
  if (length(x) == 0 || length(y) == 0 || length(x) == 1 & length(y) == 1){
    return(base_case(x, y))
  }
  # Split up into prefixes and suffixes
  xlen <- length(x)
  ylen <- length(y)
  px <- x[1:xlen - 1]
  py <- y[1:ylen - 1]
  ex <- x[xlen]
  ey <- y[ylen]
  
  # Computing three alignments
  A <- global_aln(px, py)
  B <- global_aln(c(px, ex), py)
  C <- global_aln(px, c(py, ey))
  
  # Compiling scores
  answerA <- list(x = x, y = y, 
                  xaln = c(A[["xaln"]], ex),
                  yaln = c(A[["yaln"]], ey),
                  score = A[["score"]] + score_aln(ex, ey))
  answerB <- list(x = x, y = y, 
                  xaln = c(B[["xaln"]], "-"),
                  yaln = c(B[["yaln"]], ey),
                  score = B[["score"]] + score_aln("-", ey))
  answerC <- list(x = x, y = y, 
                  xaln = c(C[["xaln"]], ex),
                  yaln = c(C[["yaln"]], "-"),
                  score = C[["score"]] + score_aln(ex, "-"))
  
  # finding the best score
  bestanswer <- answerA
  bestscore  <- answerA[["score"]]
  if (answerB[["score"]] > bestscore){
    bestanswer <- answerB
    bestscore  <- answerB[["score"]]
  }
  if (answerC[["score"]] > bestscore){
    bestanswer <- answerC
    bestscore  <- answerC[["score"]]
  }
  return(bestanswer)
}

#' #### Testing Recursive function
x <- char_vec("TATCGG")
y <- char_vec("TCTGG")
system.time(answer <- global_aln(x, y))
print(answer)

#' Testing with bigger sequence
#' 
x <- char_vec("TATCGGT")
y <- char_vec("TCTGGTCC")
system.time(answer <- global_aln(x, y))
print(answer)
#' 
#' ### Memoized recursive function
#' 
#' Because the search tree has to go down the same damn paths over and over again,
#' it would be better to memoize this so that the call stack gets smaller.
#' 
#' This is best acheived with a hash where the keys are both inputs. They can be
#' separated by a comma such as `"TATC,TCTG"` would be the key that would produce
#+ echo = FALSE
list(x = "TATC", y = "TCTG", 
     xaln = "TATC", 
     yaln = "TCTG", 
     score = -3)
#' 
#' Let's modify the function to do this.
#' 
#' First, set up the cache.
ALN_CACHE <<- hash()

global_aln <- function(x, y){
  thiscall <- str_c(unvec_char(x), ",", unvec_char(y))
  if (has.key(thiscall, ALN_CACHE)){
    return(ALN_CACHE[[thiscall]])
  }
  if (length(x) == 0 || length(y) == 0 || length(x) == 1 & length(y) == 1){
    return(base_case(x, y))
  }
  # Split up into prefixes and suffixes
  xlen <- length(x)
  ylen <- length(y)
  px <- x[1:xlen - 1]
  py <- y[1:ylen - 1]
  ex <- x[xlen]
  ey <- y[ylen]
  
  # Computing three alignments
  A <- global_aln(px, py)
  B <- global_aln(c(px, ex), py)
  C <- global_aln(px, c(py, ey))
  
  # Compiling scores
  answerA <- list(x = x, y = y, 
                  xaln = c(A[["xaln"]], ex),
                  yaln = c(A[["yaln"]], ey),
                  score = A[["score"]] + score_aln(ex, ey))
  answerB <- list(x = x, y = y, 
                  xaln = c(B[["xaln"]], "-"),
                  yaln = c(B[["yaln"]], ey),
                  score = B[["score"]] + score_aln("-", ey))
  answerC <- list(x = x, y = y, 
                  xaln = c(C[["xaln"]], ex),
                  yaln = c(C[["yaln"]], "-"),
                  score = C[["score"]] + score_aln(ex, "-"))
  
  # finding the best score
  bestanswer <- answerA
  bestscore  <- answerA[["score"]]
  if (answerB[["score"]] > bestscore){
    bestanswer <- answerB
    bestscore  <- answerB[["score"]]
  }
  if (answerC[["score"]] > bestscore){
    bestanswer <- answerC
    bestscore  <- answerC[["score"]]
  }
  ALN_CACHE[[thiscall]] <<- bestanswer
  return(bestanswer)
}
#'
#' #### Testing memoized function
#' 
x <- char_vec("TATCGGTCTA")
y <- char_vec("TCTGGTCCAC")
system.time(answer <- global_aln(x, y))
print(answer)