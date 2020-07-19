
(1 + 2^-52) - 1
(1 + 2^-53) - 1

.Machine$double.eps
2^-52

x <- 0.1 + 0.2 - 0.3
x

x == 0

isTRUE(all.equal(x, 0))
abs(x) < 1e-12

sqrt(.Machine$double.eps)

all.equal(list(), 7)
isTRUE(all.equal(list(), 7))

A<-matrix(c(1,2,2,1),2,2)
b<-matrix(c(1,1),2,1)

x <- solve(A) %*% b
A %*% x

data(trees)
head(trees)
nrow(trees)
pairs(trees)

lm1 <- lm(Volume ~ Height + Girth, data=trees)
summary(lm1)

X = cbind(1, trees$Height, trees$Girth)

beta = qr.solve(X, trees$Volume)
beta
