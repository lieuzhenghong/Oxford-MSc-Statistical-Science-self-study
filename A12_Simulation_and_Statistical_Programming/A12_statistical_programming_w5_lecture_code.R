#MAT OPS

A <- matrix(1:9, 3, 3)  # create matrix
dim(A)
t(A)                    # transpose of A
det(A)                  # determinant of A
A %*% c(3,5,7)          # matrix multiplication
A[2]
A[2,]                   # subsetting
A[2,,drop=FALSE]        # keeps answer as a matrix
rbind(1, A, A)          # see also cbind()
A[-1,-2]                # removes 1st row and 2nd column

col(A)                  # matrix of column numbers
row(A)
diag(A)                 # diagonal entries
diag(A) <- c(9,9,4)
upper.tri(A)
A[upper.tri(A)] = 0     # what's happened to A?


#recursion
myFactorial <- function(x) {
  if (x == 0) return(1)
  if (x > 0) return(x*myFactorial(x-1))
  stop("x must be a positive integer")
}

#an expensive alg for det!
getDet <- function(M) {
  out <- 0
  n <- nrow(M)
  if (n == 1) return(M[1,1])  ## base case n = 1
  
  for (i in 1:n) {
    ## get determinants of each minor
    tmp <- M[1,i] * getDet(M[-1, -i, drop=FALSE])
    out <- out + (-1)^(i-1) * tmp
  }
  out
}

n<-10; dM<-getDet(M<-matrix(rnorm(n*n),n,n)); dM
det(M) #a wee bit faster!


