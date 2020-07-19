2^1023
2^1024
2^(-1074)
2^(-1075)

set.seed(153524)
x <- rnorm(1000)
Lx <- dnorm(x)
min(Lx)    # likelihood for each observation is positive
prod(Lx)   # total likelihood appears to be zero

sum(log(Lx))   # log-likelihood is managable
sum(dnorm(x,log=TRUE))

log(exp(1000) - exp(999) + exp(998) - exp(997))

log(exp(3) - exp(2) + exp(1) - exp(0)) + 997

log(factorial(500))
lfactorial(500)

dnorm(10, 0, 1)
dnorm(10, 0, 1, log=TRUE)



