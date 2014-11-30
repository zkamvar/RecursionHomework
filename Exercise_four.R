#' Sequence Alignment
#' ===========
#' 
#' ## Required Packages
library(stringr)
library(rstackdeque)
library(hash)
#'
#' ## Pre-written Functions
#' 
#' Given a character vector of length 1, this will split it into a vector of 
#' length(nchar(string)).
#' 
#' Example:
#'  - Input:  "ATCG"
#'  - Output: c("A", "T", "C", "G")
char_vec <- function(string){
  if (length(string) == 1 & string == ""){
    return(c())
  }
  c_vec <- str_split(string, "")[[1]]
  c_vec <- c_vec[seq(2, length(c_vec))]
  return(c_vec)
}
#'
#' Given a vector of single character letters, join them into a character vector
#' of length 1.
unvec_char <- function(a){
  ret <- str_c(a, collapse = "")
  return(ret)
}
#' 
#' ### Scoring Functions
#' 
#' 
#' #### Compute the score of an alignment
#' Inputs:
#'  - xin: a character vector
#'  - yin: a character vector
#'  - match: a score for matching sequences
#'  - gap: a score for sequences with gaps
#'  - mismatch: a score for non-matching sequences
#'  
score_aln <- function(xin, yin, match = 2, gap = -4, mismatch = -3){
  if (length(xin) != length(yin)){
    stop("Can't score sequences of unequal length.")
  }
  score <- 0
  for (i in seq(1, length(xin))){
    xi <- xin[i]
    yi <- yin[i]
    if (xi == yi){
      score <- score + match
    } else if (xi == "-" || yi == "-"){
      score <- score + gap
    } else {
      score <- score + mismatch
    }
  }
  return(score)
}
#'
#' #### Compute the base case
base_case <- function(xin, yin){
  if (length(xin) > 1 & length(yin) != 0 || length(yin) > 1 & length(xin) != 0){
    stop("This is not a base case.")
  } else if (length(xin) == 1 & length(yin) == 1){
    answer <- list(x = xin, y = yin,
                   xaln = xin, yaln = yin,
                   score = score_aln(xin, yin))
    return(answer)
  } else if (length(xin) == 0){
#     xaligned <- c()
    xaligned <- yin
    yaligned <- yin
#     for (i in seq(1, length(yin))){
#       xaligned <- c(xaligned, "-")
#     }
    xaligned[] <- "-"
    answer <- list(x = xin, y = yin,
                   xaln = xaligned, yaln = yaligned,
                   score = score_aln(xaligned, yaligned))    
    return(answer)
  } else if (length(yin) == 0){
#     yaligned <- c()
    yaligned <- xin
    xaligned <- xin
#     for (i in seq(1, length(xin))){
#       yaligned <- c(yaligned, "-")
#     } 
    yaligned[] <- "-"
    answer <- list(x = xin, y = yin,
                   xaln = xaligned, yaln = yaligned,
                   score = score_aln(xaligned, yaligned))    
    return(answer)
  } else {
    stop("How did you even get here?")
  }
}
#'
#' ## Testing
#' 
#' ### Testing base case one: sequence and gap.
xpre <- "TAC"
ypre <- ""

x <- char_vec(xpre)
y <- char_vec(ypre)
base_case_answer_1 <- base_case(x, y)
print(base_case_answer_1)
#'
#' ### Testing base case two: mismatch.
xpre <- "T"
ypre <- "A"

x <- char_vec(xpre)
y <- char_vec(ypre)
base_case_answer_2 <- base_case(x, y)
print(base_case_answer_2)