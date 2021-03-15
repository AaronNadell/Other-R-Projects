library(readxl)
library(ggplot2)
load("Template for R.xlsx")
Template_for_R <- read.csv("Documents/R/Template for R.csv")

View(Template_for_R)
ggplot(data= Template_for_R) +
  geom_point(aes(x=data1, y=data2))

ggplot(data= Template_for_R) +
  geom_point(aes(x=data1, y=data3))

ggplot(data= Template_for_R) +
  geom_point(aes(x=data1, y=data2, size = type))

#This is the one
ggplot(data= Template_for_R) +
  geom_point(aes(x=data1, y=data2, color= type))

#ggplot(data=Template_for_R) + geom_point(mapping = aes(x=data1, y=data2))
#+ facet-wrap(~type, nrow = 2)

ggplot(data=Template_for_R) + geom_smooth(mapping = aes(x=data1, y=data2))

ggplot(data=Template_for_R) +
  geom_smooth(mapping = aes(x=data1, y=data2, linetype = type))

ggplot(data=Template_for_R)+ geom_bar(mapping= aes(x=type, y=data1))
ggplot(data=Template_for_R)+ geom_bar(mapping= aes(x=type))
ggplot(data=Template_for_R)+ stat_summary(mapping = aes(x=type, y=data1),
                                          fun.ymin = min,
                                          fun.ymax = max,
                                          fun.y = median)

jobsAFAM1 <- data.frame(
  data_date = runif(5,1,100),
  Percent.Change = runif(5,1,100)
)

jobsAFAM2 <- data.frame(
  data_date = runif(5,1,100),
  Percent.Change = runif(5,1,100)
)

ggplot() + 
  geom_line(data = jobsAFAM1, aes(x = data_date, y = Percent.Change), color = "red") +
  geom_line(data = jobsAFAM2, aes(x = data_date, y = Percent.Change), color = "blue") +
  xlab('data_date') +
  ylab('percent.change')
plot(iris)

