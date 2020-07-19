#object classes
class(pi)
class(TRUE)

#strings
x <- "Hello"
x
class(x)

x <- c("This", "is", "the first", "day",
       "of the rest", "of your life!")
x[3]

#operations on strings
paste(x, collapse=" ")
cat(x, sep=" ")
gsub("i","I",x)

#factors
agree <- c("Y", "N", "Y", "S", "Y", "N", "N","S","S")
Agree <- factor(agree)
agree
Agree
levels(Agree)
class(Agree)

#plotting factors
x <- rnorm(length(Agree))
plot(Agree, x) #R does something sensible when given a factor to plot
boxplot(x~Agree) #same thing but handles multiple factors
plot(x, Agree,col=Agree)  #converts levels to numeric

#loading data
setwd("../practicals/")
hd <- read.table("hellung.txt", header=TRUE)

#over the network
con<-url("http://www.stats.ox.ac.uk/~nicholls/PartASSP/hellung.txt")
hd <- read.table(con, header=TRUE)
close(con)

hd <- read.table("http://www.stats.ox.ac.uk/~nicholls/PartASSP/hellung.txt", header=TRUE)

#functions on data frames
class(hd)
is.data.frame(hd)
is.list(hd)
is.matrix(hd)
is.numeric(hd)
is.numeric(hd[,1])

dim(hd)
names(hd)
length(hd)

head(hd, 3)
hd$diameter[1:10]
hd[2,2]
hd[3:4,]

#explore data frame
str(hd)
summary(hd)
plot(hd)

x = log(hd$conc)
y = hd$diameter
plot(x, y, xlab="log(conc)", ylab="cell diameter")

plot(log(conc), diameter)
plot(log(hd$conc), hd$diameter)

with(hd, plot(log(conc), diameter)) 
plot(diameter~log(conc), data=hd)

#apply a function to the columns of hd
apply(hd, 2, max)

#select entries and average
which(hd$glucose==1)
mean(hd$diameter[which(hd$glucose==1)])
mean(hd$diameter[hd$glucose==1])

#subset
hd[hd$conc > 550000,]

hd[1,]
hd[1,3] <- 21
hd[1,]

#adding columns
hd$random1 <- rnorm(51)
head(hd, 3)
hd <- cbind(hd, random2 = rnorm(51))
head(hd, 3)

my.df=data.frame(apples=c(T,F,F,T,T,T,F),
                 color=c("red","orange","red","red","green","red","orange"),
                 weight=c(160,143,123,171,157,170,150))
my.df
str(my.df)

#missing values
x <- c(NA, 5, 9, NA, 7)
x
is.na(x)
any(is.na(x))
any(is.na(hd))

mean(x)
mean(x, na.rm=TRUE)



#some fun illustrating OO aspects - outside scope of this course

#overwriting the + operator and adding a new method
`+` <- function (e1, e2) UseMethod("+")
`+.default` <- function (e1, e2) .Primitive("+")(e1, e2)
`+.character` <- function(e1, e2) paste(e1, e2, sep = '')

1+1
"The quick " + "brown fox " + "jumped over " + "the lazy dog"

`-` <- function (e1, e2) UseMethod("-")
`-.default` <- function (e1, e2) .Primitive("-")(e1, e2)
`-.character` <- function(e1, e2) gsub(e2,"",e1)

"Lost Consonants" - "on"

"Lost Consonants" - "Conson" + " found!" 

