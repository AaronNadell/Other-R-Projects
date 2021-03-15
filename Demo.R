#Practice with R
#January 27th
library(mosaic)
library(ggplot2)

3+2

sqrt(4)

x <- c(1:10)
x[4]
seq(1,10, by=1)
seq(1,10, by=2)
seq(1,10, length = 2)
seq(1,10, length = 3)

myvector <-seq(5,15)
myvector[3]
myvector[-1] #takes away an element
myvector[c(1,4)] #takes the first and 4th element

sum(myvector)
sort(c(2,5,3,20,19,2,1,10))

c("orange", "apple", "grapes")
z <- c("orange", "apple", "grapes")

matrix(c("A", "B", "C", "D", "E", "F"), nrow =3 )
data <- read.csv("/Users/Aaron/Documents/Applied Statistics/Movies.csv") #option copy then paste this pastes
                                                                          #the directory into the function
head(data) #gives first six rows of each category
tail(data) #gives the last six rows of each category
summary(data)
summary(data$Budget)
str(data)
names(data)
nrow(data)
ncol(data)
dim(data)
mean(data$RunTime)

hist(data$RunTime)

pdf(file = "myhistogram.pdf")
hist(data$RunTime)
dev.off()

postscript(file = "myhistogram.ps")
hist(data$RunTime)
dev.off()
##############random things I'm doing################
x <- sample(1:100, 20, replace =T)
y <- sample(1:10, 20, replace = T)

data1 <- data_frame(
  "x" <- x,
  "y" <- y
)

plot(x, y, type = "o", main = "Yearly Income", xlab = "Year", ylab = "Income")

a <- ggplot() + geom_line(data = data, aes(x=x, y=y))



