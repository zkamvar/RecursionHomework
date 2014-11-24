#' Sequence alignment
#' ===========
#' 
#' Nothing terribly new. 
#' 
#' ### Talking about proofs
#' 
#' Basically a really convincing argument.
#' 
#' #### Proof by induction
#' 
#' We set some base cases that we know are true and utilize those to leverage future
#' cases to being true...
#' 
#' #### Proof by contradiction
#' 
#' Assume the opposite of what you want to prove. Argue to contradiction.
#' 
#' > Theorem: There's no smallest integer.
#' 
#' Assume that there is a smallest integer: $i$
#' If this is true and all math is still valid, then there is also $i - 1$, 
#' which means that $i - 1 < i$, thus there is no smallest integer.
#' 
#' > Theorem: If a car's batterey is dead and the key is turned, then the car
#' won't start.
#' 
#' ##### Proof
#' 
#' Assume (for a proof by contradiction) that the car does start, but the 
#' battery is dead and the key is turned. 
#' 
#' 1. Car starts
#' 2. Starter turned
#' 3. Starter uses electricity
#' 4. Source of electricity: battery
#' 5. Battery is dead. This is a contradiction.
#' 
#' If you have a proof by contradiction, you can pick which assumption is false. 
#' 
#' Global Sequence Alignment
#' ========
#' 
#' X: ACTAGC
#' 
#' Y: ATACC
#' 
#' produce an alignment:
#' 
#' ```
#' ACTAGC
#' | || |
#' A-TACC
#' ```
#' 
#' We want maximum parsimony. How do we score it?
#' 
#'  - Match = +2
#'  - Mismatch = -3
#'  - gap = -4 
#' 
#' Score of above alignment: $2 - 4 + 2 + 2 - 3 + 2 = `r 2 - 4 + 2 + 2 - 3 + 2`$
#' 
#' Rules: gap < mismatch < match
#' 
#' Let's assume that each sequence is 1 base each.
#' 
#' X: A; A; A;    ;
#' 
#' Y: A; T;  ; ATC;
#' 
#' Alignment :
#' 
#' ```
#' A; A; A; ---;
#' A; T; -; ATC;
#' ```
#' 
#' Let's take the original alignment. Everything but the last base in $Y$ will be $P_y$.
#' Everything but the last base in $X$ $P_x$. The last bases in both will be $e_y$ and 
#' $e_x$, respectively.
#' 
#' When you have this, you essentially have three hypotheses (I'm using $:$ to denote
#' "aligns with"):
#' 
#' $$
#' A = (P_x:P_y)e_x:e_y\\
#' B = (X:P_y)-:e_y\\
#' C = (P_x:Y)e_x:-\\
#' $$
#' 
#' best alignment of X to Y is the best of the three scores. This ends up being a 
#' recursive problem because you have to figure out the scores of A, B, and C. 
#' 
#' How do we prove this? Proof by induction.
#' 
#' ##### Base case
#' 
#'  - X and Y are length 1, alignment is X:Y.
#'  - if X or Y is length $0$, no choice. Optimal is filling one with gaps.
#' 
#' ##### Inductive case
#' 
#' Assume that A, B, and C have been computed optimally. Prove that the max of the 
#' 3 scores is optimal.
#' 
#' Proof by contradiction: 
#' 
#'  - suppose that A, B, and C is not optimal. 
#'  - The optimal is $Z'$.
#'  
#' $Z'$ must be one of 
#'  
#' $$
#' ( )e_x:e_y\\
#' ( )e_x:-\\
#' ( )-:e_y\\
#' $$
#' 
#' Without loss of genarlity (wlog) (ie - we're going to argue one case, others
#' will be argued the same way).
#' 
#' Suppose it's $Z'()e_x:e_y$
#' 
#' $Score(Z') = S(Z) + S(e_x, e_y)$. By assumption, this less than the max of our three
#' $< S(A) + S(e_x, e_y)$ implies that A is NOT optimal.
#'