addNumbers <- function(x, y)
{
  # body of code here
  z <- x + y
  return(z)
}
addNumbers(3,5)

## Function to calculate p-norm of a vector
p_norm <- function(x, p=2)
{
  modx <- abs(x)
  z <- sum(modx^p)
  return(z^(1/p))
}
p_norm(c(3,4))
p_norm(c(3,4), 1)

## Function to get even numbered entries in vector
evenElements <- function(x)
{
  len <- length(x)
  sq <- seq(from=2, to=len, by=2)
  return(x[sq])
}
evenElements(c(1,3,5,7,9,11))
evenElements(4)

## Function to get even numbered entries in vector
evenElements <- function(x)
{
  len <- length(x)
  if (len < 2) return(numeric(0))
  sq <- seq(from=2, to=len, by=2)
  return(x[sq])
}
evenElements(4)

#for loops
N<-10e+7; 
system.time({tot<-sum((1:N)^2)}); tot
system.time({tot=0; 
             for (k in 1:N) {
               tot<-tot+k^2
               }
            }); tot

#if
mod <- function(x)
{
  if (x < 0)
  {
    out <- -x
  }
  else
  {
    out <- x
  }
  out
}
mod(-4)

#for
factorial2 <- function(n)
{
  out <- 1
  for (i in 1:n)
  {
    out <- out*i
  }
  out
}
factorial2(10)

#while
rTruncNorm <- function(a)
{
  #Let Z~N(0,1) - simulate Z|Z>a 
  z <- a - 1
  while (z < a)
  {
    z <- rnorm(1)
  }
  z
}
rTruncNorm(2)
#rTruncNorm(10)

isPr <- function(x)
{
  M <- floor(sqrt(x))   # what does this do?
  out <- TRUE
  for (i in seq(2, M))
  {
    if (x %% i == 0)
    {
      out <- FALSE
      break
    }
  }
  out
}
isPr(5)
isPr(8)

#function pointers
newtonRaphson <- function(f, f.prime, x, tol = 1e-8)
{
  #Newton-Raphson iteration for f
  while (abs(f(x)) > tol)
  {
    x = x - (f(x) / f.prime(x))
  }
  return(x)
}
f <- function(x) x^3 + 2*x^2 - 7
f.prime <- function(x) 3*x^2 + 4*x
x<-newtonRaphson(f, f.prime, 2); x
f(x)

add <- function(x, y)
{
  z <- x + y
  return(z)
}
add(3,5)
z #not in this environment

a <- 5
f <- function() {
  cat("a =", a, "\n")  
  # cat() prints out its arguments
  a <- a + 1
  cat("a =", a, "\n")
  return(a)
}
f() #local copy of a in f() changes
a #doesnt change in this environment

#more scope
z <- 0
f1 <- function ()
{
  # you can define functions inside other functions
  f2 <- function()
  {
    print(z)
  }
  z <- 1
  f2()
}
f1()

#apply
X <- matrix(c(1,2,3,4,5,6),2,3)
X
apply(X, 1, sum) #sum rows
apply(X, 2, max) #sum columns


f<-function(x) sum(x<2.5)
apply(X, 2, f)
apply(X, 2, function(x) sum(x<2.5))

