
#Importing needed functions
library(plyr)
cbind.fill <- function(...){  #Found this on stack Overflow
  nm <- list(...) 
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}

#################### Initializing Dataframes #######################
a1 <- "a1" 
a2 <- "a2"

column1 <- c(a1,a1,a1,a2,a2,a2)
column2 <- c(1,1,1,1,1,1)

#creating dataframe with variables
data1 <- data.frame(column1, column2) 

a3 <- "a3"
a4 <- "a4"
b1 <- "b1"
b2 <- "b2"

#creating dataframe explicitly by defining vectors within the data.frame function
data2 <- data.frame(column1 = c(a3,a3,a3,a3,a3,a3,a4,a4,a4), 
                    column2 = c(1,1,1,1,1,1,1,1,1),
                    column3 = c(b1,b1,b1,b2,b2,b2,b2,b2,b2))

#Printing what data1 and data2 look like
data1
data2 

#################### rbinding and cbinding dataframes #######################
rbind_data12 <- rbind.fill(data1, data2) 

cbind_data12 <- cbind.fill(data1, data2)

################### Parsing needed data (the long way) #####################

#Example...
c(data1$column1, data2$column1) #creates a new string with desired data


#In Practice: Create new dataframe with desired data
data3 <- data.frame(c(data1$column1, data2$column1), 
                    c(data1$column2, data2$column2)) 
data3




