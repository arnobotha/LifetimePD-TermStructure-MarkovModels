
# --- Continuous-time Markov Chain

# - Dummy date
# Number of transitions from each state to each destination state over sampling window
matTransitions <- matrix(c(0, 5, 3, 2,  # Transition counts from state 1 to others
              4, 0, 6, 3,  # Transition counts from state 2 to others
              2, 7, 0, 4,  # Transition counts from state 3 to others
              3, 4, 5, 0), # Transition counts from state 4 to others
            nrow = 4, byrow = TRUE)

# Total holding times, i.e., time spent in each state
vecTimeSPent <- c(100, 120, 110, 90)


# -- Generator matrix: Part 1
# Estimate off-diagonal transition rates, i.e., how quickly exits occur from the
# kth row (start state) to lth column (destination state) for k \ne l
# NOTE: Lambda is used in continuous-time context in representing the instantaneous
# transition rate of the system at t, \lambda_kl >= 0; which does not render them
# as probabilities necessary.
# NOTE2: Lambda_kl are entries within the "generator" matrix, which is the analogue
# of the transition matrix of a discrete-time Markov Chain.
# NOTE3: A transition matrix will contain entries (transition probabilities) that are probabilities, whereas
# a generator matrix will contain \lambda-entries (transition rates) that must first be exponentiated to 
# describe probabilities
matLambda <- matTransitions / vecTimeSPent


# -- Generator matrix: Part 2
# Set diagonal entries
# NOTE: These transition rates signify the overall speed of leaving state k
# Whilst negative by design (such that the rowsum of the Generator will always be 0),
# the absolute value of \lambda_{kk} denotes the rate at which state k is left for any other state
# Larger values implies shorter expected time spent in state k
# NOTE2: the "holding time" in state k is exponentially distributed for continuous-time Markov Chains, such that
# E[Holding Time] = 1 / \lambda_kk
diag(matLambda) <- -rowSums(matLambda)
abs(1/diag(matLambda)) # Expected holding times per state
matLambda # Generator matrix




# ------ From Generator Matrix to Transition matrix
# The transition matrix is linked to the generator matrix via the exponential matrix function
# P(t) = exp(\Lambda \cdot t) where exp is the matrix exponential function (NB!)
# NOTE: exp(matLambda*2) # INCORRECT since the matrix exponential is a type of operator
# used in solving linear differential equations, or indeed, continuous-time Markov chains.
# It is an operation that transforms a square matrix A into another matrix e^A, analogous
# to the scalar exponential function e^x.
# This operatoer e^A is defined using Taylor series expansion as
# e^A = I + A + A^2/2! + A^3/3! + ... , where I is the identity matrix of the same size
# as A, A^k is the kth power of A, and k! is the factorial of k.

# Function to compute the matrix exponential using Taylor series expansion
matExp_Taylor <- function(matGiven, tGiven, maxTaylorTerms = 100, tol = 1e-6) {
  n <- nrow(matGiven)
  matExp <- diag(n)  # Initialize as identity matrix
  term <- diag(n)     # First term in the series (I)
  k <- 1              # Factorial denominator
  
  # NOTE: Basic Taylor series expansion is:
  # exp(\Lambda . t) = I + \Lambda.t + (\Lambda.t)^2/2! + (\Lambda.t)^3/3! + ...
  
  repeat {
    # Compute the next term in the series using matrix multiplication (%*%)
    # NOTE1: the term (term %*% (matGiven * tGiven)) calculates A^k.t^k, 
    # effectively multiplying the previous term by A.t
    # NOTE2: By multiplying the previous term with (A.t), the
    # cumulative product required for k! is mimicked by dividing simply by k, thereby
    # circumventing the need for explicitly calculating factorials (computationally expensive)
    # NOTE2: Effectively, each iteration builds up the appropriate power of t and matrix exponentiation,
    # while the division by k accumulates the necessary factorial terms for each step.
    term <- (term %*% (matGiven * tGiven)) / k
    matExp <- matExp + term
    
    # Break if the change is below the tolerance or max_terms reached
    if (max(abs(term)) < tol || k >= maxTaylorTerms) break
    
    k <- k + 1
  }
  return(matExp)
}

# Call matrix exponential on given generator function
sTime <- 1
(matTransProb <- matExp_Taylor(matLambda, tGiven=sTime))
# [SANITY CHECK] The row sum of entries should sum to 1 by definition
rowSums(matTransProb)




# ------ From Generator matrix to transition intensities
# NOTE: Transition intensities \lambda_kl bridge the gap between the generator matrix \Lambda 
# and the transition matrix P, given that the latter may be expensive to comput,
# especially for larger matrices. They approximate P( X_{t +\Delta.t} = l | X_t = k) by 
# \lambda_kl.\Delta.t if k \ne = l, otherwise by 1 + \lambda_kl.\Delta.t if k=l.
# They represent short-term transition probabilities between states k and l.
# They avoid the need for matrix exponentiation to get the transition probability matrix over some t.

# - Preliminaries
sDelta <- 1 # small time increment during which we will estimate transition probabilities


# - Initialise the transition intensity matrix.
matTIs <- matrix(0, nrow = nrow(matLambda), ncol = ncol(matLambda))

# Compute the transition intensities matrix for given small time
for (i in 1:nrow(matLambda)) {
  for (j in 1:ncol(matLambda)) {
    if (i == j) {
      matTIs[i, j] <- 1 + matLambda[i, j] * sDelta
    } else {
      matTIs[i, j] <- matLambda[i, j] * sDelta
    }
  }
}

matTIs # # Display the transition intensities matrix

