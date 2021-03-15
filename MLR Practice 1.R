library(mosaic)
library(dplyr)
library(mosaic)
library(knitr)
library(leaps)

Cars <- read.csv("/Users/Aaron/Documents/Applied Statistics/Data/Cars.csv")

names(Cars)

step.model1 <- step(lm(Price~1,data=Cars),
                    scope=~Mileage+Cyl+Liter+Doors+Cruise+Sound+Leather, direction = "forward")
summary(step.model1)

ggplot(data = step.model1) + 
  geom_point(mapping = aes(x=Mileage, y=step.model1$residuals))
#There is still a downward trend in the outliers that are above 20k, and a general right skewness
#1c. The residuals at mileage = 8k are not balanced around y=0 because there is a right skewness. 
#The outliers mentioned before are causing this right skewness.
ggplot(data = step.model1) + 
  geom_point(mapping = aes(x=Cyl, y=step.model1$residuals))
#The ponts show a normal residuals for Cyl=4,6 and right skewness for Cyl=8
ggplot(data = step.model1) + 
  geom_point(mapping = aes(x=Doors, y=step.model1$residuals))
#There's right skewness for two door cars but no skewness for 4doors.
ggplot(data = step.model1) + 
  geom_point(mapping = aes(x=Cruise, y=step.model1$residuals))
# There's no skew in the residuals for cars without cruise but slight right skewness
#for cars with cruise.
ggplot(data = step.model1) + 
  geom_point(mapping = aes(x=Sound, y=step.model1$residuals))
#Both residual plots may show right skewness.
ggplot(data = step.model1) + 
  geom_point(mapping = aes(x=Leather, y=step.model1$residuals))
#Both residual plots show slight right skewness as well with the one's 
#with leather showing greater right skewness
head(step.model1)

ggplot(Cars, aes(step.model1$residuals, step.model1$fitted.values)) + 
  geom_point(mapping = aes(x = step.model1$residuals, y = step.model1$fitted.values))
#There is definitely not homoskedasticity as there is a clear pattern. The variances do not
#appear to be the same. There appears to be an upward trend in the  residuals as the fitted values
#increase. This likely means that the model becomes worse for higher fitted values.

####### Transformations ###########
Cars$logprice <- log(Cars$Price)
Cars$sqrtprice <- sqrt(Cars$Price)

######## Log ########
step.model2 <- step(lm(logprice~1,data=Cars),
                    scope=~Mileage+Cyl+Liter+Doors+Cruise+Sound+Leather, direction = "forward")
summary(step.model2)
ggplot(data = step.model2) + 
  geom_point(mapping = aes(x=Mileage, y=step.model2$residuals))
#The residual is still right skewed.
ggplot(data = step.model2) + 
  geom_point(mapping = aes(x=Cyl, y=step.model2$residuals))
#still skewed right for cyl = 8
ggplot(data = step.model2) + 
  geom_point(mapping = aes(x=Doors, y=step.model2$residuals))
#There's right skewness for two door cars
ggplot(data = step.model2) + 
  geom_point(mapping = aes(x=Cruise, y=step.model2$residuals))
# There's no skew in the residuals for cars without cruise but slight right skewness
#for cars with cruise.
ggplot(data = step.model2) + 
  geom_point(mapping = aes(x=Sound, y=step.model2$residuals))
#Only cars with sould have right skewness.
ggplot(data = step.model2) + 
  geom_point(mapping = aes(x=Leather, y=step.model2$residuals))
#Cars without leather have right skew but those with have been fixed.
###### Sqrt ######
step.model3 <- step(lm(sqrtprice~1,data=Cars),
                    scope=~Mileage+Cyl+Liter+Doors+Cruise+Sound+Leather, direction = "forward")
summary(step.model3)
ggplot(data = step.model3) + 
  geom_point(mapping = aes(x=Mileage, y=step.model3$residuals))
#There is still a downward trend in the outliers that are above 20k, but there
#appears to be some right skew overall
ggplot(data = step.model3) + 
  geom_point(mapping = aes(x=Cyl, y=step.model3$residuals))
#The ponts show a normal residuals for Cyl=4,6 and right skewness for Cyl=8
ggplot(data = step.model3) + 
  geom_point(mapping = aes(x=Doors, y=step.model3$residuals))
#There's right skewness for two door cars but no skewness for 4doors.
ggplot(data = step.model3) + 
  geom_point(mapping = aes(x=Cruise, y=step.model3$residuals))
# There's no skew in the residuals for cars without cruise but slight right skewness
#for cars with cruise.
ggplot(data = step.model3) + 
  geom_point(mapping = aes(x=Sound, y=step.model3$residuals))
#only cars with sound have a right skewness
ggplot(data = step.model3) + 
  geom_point(mapping = aes(x=Leather, y=step.model3$residuals))

###### Comparison of the models ######
#The Adjusted R-Squared for the log plots is 0.4818 and the adjust R-squared is 0.4649 for
#the sqrt transformed plots. They both had little effect on the skewness however I would say that 
#the log transformation outperformed the sqrt overall because it normalized the outliers in the
#mileage vs residual plot but the sqrt didn't, however they performed similarly on the other
#graphs. Yes, in this case the log transformation had a larger R squared value and corrected the
#skewness in the residuals better. This makes sense as correcting the skewness might mean that
#the model is better able to predict because of the reduced variability.

################ 3 ###############
MileagevsLprice <- lm(logprice~Mileage, data=Cars)
summary(MileagevsLprice)
plot(MileagevsLprice$residuals, 1:length(MileagevsLprice$residuals))

MileagevsSprice <- lm(sqrtprice~Mileage, data=Cars)
summary(MileagevsSprice)
plot(MileagevsSprice$residuals, 1:length(MileagevsSprice$residuals))
#There are very clear patterns occuring in these residual models for the order. 

#maybe the model of the car is having a significant effect on the price of the cars
#this would make sense because they are listed in clumps of models and it was not accounted for 
#by our model.

#4. I see another pattern that looks a little different, however this could just be an error
#in the way I calculated it.
plot(step.model2$residuals, 1:length(step.model2$residuals))
ggplot(Cars, aes(x=c(1:nrow(Cars)), y=step.model2$residuals)) + geom_point() + geom_line() + geom_hline(aes(yintercept =0))
#5. 
models <- regsubsets(Price~Mileage+Cyl+Liter+Doors+Cruise+Sound+Leather, data = Cars, nvmax = 6)
summary(models)
final.model <- lm(Price~Mileage+Cyl+Doors+Cruise+Sound+Leather, data=Cars)
final.model.equation <- 7323.16 + -0.171(mileage) + 3200(Cyl) + -1463.4(Doors) + 6205.5(Cruise) + -2024.4(Sound) + 3327.14(Leather)
plot(final.model$residuals, final.model$fitted.values)
#there seems to be a downward trend in the the residuals as the fitted values increase.
#additionally there is a clump of resiudals above 0 around 25k and an increased variability in residuals
#above 30k.
plot(final.model$residuals, Cars$Mileage)
#There seems to be a trend towards more negative residuals however there are a few outliers
#at 30k+ residuals for almost every mileage. The outliers appear to be the XLR-V8 class of cas corresponding
#to rows 151 to 160. It gives a gint as to what general category might be causing the fluctuations in price
#that are unaccounted for in our model, so once we have identified them as outliers we might want to 
#incorporate model type into our model to reduce the clustering of residuals.

Cars$Resid <- step.model1$residuals
Out <- subset(Cars, Resid >20000)
Out[,c(1:6)]

filter.Cars <- Cars %>% filter(Cars$Model != "XLR-V8")
models2 <- regsubsets(Price~Mileage+Cyl+Liter+Doors+Cruise+Sound+Leather, data = filter.Cars, nvmax = 6)
summary(models2)
filter.Model <- lm(Price~Mileage+Cyl+Liter+Cruise+Sound+Leather, data=filter.Cars)
plot(filter.Model$residuals, filter.Model$fitted.values)
plot(filter.Model$residuals, filter.Model$Mileage)
filter.eq <- 8018 + -0.159(Mileage) + 524.36(Cyl) + 2708.8(Liter) + Cruise(6119.9) + -2584.1(sound) + 3041.07(Leather)
#this absolutely changed the correlation coefficients. It even changed which variables were best able
#to predict the variability in price after rerunning the best subset model.

step.model2 <- step(lm(logprice~1,data=Cars),
                    scope=~Mileage+Cyl+Liter+Doors+Cruise+Sound+Leather, direction = "forward")
summary(step.model2)

hist(step.model2$residuals)
ggplot(Cars, aes(sample = logprice)) + stat_qq() + stat_qq_line()
qqnorm(step.model2$residuals)
qqline(step.model2$residuals)
#there seems to some outliers in the normal probability plot as the left tail end of the
#data is coming off of the plot. However the data appears to be approx. normal.

