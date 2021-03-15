library(mosaic)
library(dplyr)
library(mosaic)
library(knitr)
library(leaps)

Cars <- read.csv("/Users/Aaron/Documents/Applied Statistics/Data/Cars.csv")

PML <- lm(Price~Mileage + Liter, data = Cars)
summary(PML)
#R-squared = 0.3291
#Liter is quite important based on high t-value and small p-value
PMC <- lm(Price~Mileage + Cyl, data = Cars)
summary(PMC)
#R-squared = 0.3398
#Cyl is quite important based on high t-value and small p-value
PMLC <- lm(Price~Mileage + Liter + Cyl, data = Cars)
summary(PMLC)
#R-squared = 0.3423
#Liter is less imporant because the t-value has dropped and the p-value increased
PML_EQ <- 9427 + 4968(Liter) - 0.16(Mileage)
PMC_EQ <- 3146 + 4028(Cyl) - 0.152(Mileage)
PMLC <- 4708 + 1545(Liter) + 2848(Cyl) - 0.154(Mileage)
#The liter coefficient drops when Cyl is included in the model. Liter drops by about 3500 but
#Cyl drops by about 1200.

ggplot(Cars, aes(x=Liter, y=Cyl)) + geom_point(color="orange")
cor(Cars$Cyl, Cars$Liter)
#There is a strong correlation between Cyl and Liters so it makes sense that you can exclude one or the 
#other.

Cars$logprice <- log(Cars$Price)
Cars$sqrtprice <- sqrt(Cars$Price)

boxplot(Cars$logprice, Cars$Make)
#The make of the car has a wider distribution than the logprice and median way lower than logprice with
#a few outliers on either side.
boxplot(Cars$logprice, Cars$Model)
#This distribution is way wider with median around 15, but no outliers.
boxplot(Cars$logprice, Cars$Trim)
#This distribution is wider with median around 30, but no outliers and a little skewed left
boxplot(Cars$logprice, Cars$Type)
#It looks like there's a median of 4 and heavily left skewed with an outlier below it.

lm1<-lm(logprice~Mileage, data=Cars)
summary(lm1)
#adjusted R-squared = 0.02069
lm2<-lm(logprice~Liter, data=Cars)
summary(lm2)
#adjusted R-squared = 0.3478
lm3<-lm(logprice~Make, data=Cars)
summary(lm3)
#adjusted R-squared = 0.6316
lm4<-lm(logprice~Mileage + Make, data=Cars)
summary(lm4)
#adjusted R-squared = 0.6546
lm5<-lm(logprice~Liter + Make, data=Cars)
summary(lm5)
#adjusted R-squared = 0.8906
#The R-squared values don't just add together but they do increase to almost the sum of both in each
#case. I assume that the percent explained by each variable for each variable you add decreases by some
#factor. 
lm6 <-lm(logrpice~Liter+Make+Mileage, data=Cars)
summary(lm6)
#adusted R-squared = 0.8492 This model is worse than the one that just used Make and Liter.
qqnorm(lm6$residuals)
qqline(lm6$residuals)
#yes the normal plot suggests that the data follow a normal line suggesting the distribution is normal.
plot(lm6$residuals, lm6$fitted.values)
#There appears to be an even distribution across the residual =0 line with tight clumping when
#the fitted value =0.
plot(lm6$residuals, 1:length(lm6$residuals))
# There are no clear patterns in this data and it appears pretty random.
plot(lm6$residuals, Cars$Liter)
plot(lm6$residuals, Cars$Make)
plot(lm6$residuals, Cars$Mileage)

#inclass notes:
cor(Cars$Cyl, Cars$Liter)
ggplot(Cars, aes(Make, logprice)) + geom_point()
ggplot(Cars,aes(Model, logprice)) + geom_point(aes(color=Make))+
  theme(axis.text.x=element_text(angle=45,hjust=1))
ggplot(Cars,aes(Trim, logprice)) + geom_point(aes(color=Make))+
  theme(axis.text.x=element_text(angle=90,hjust=1))
ggplot(Cars, aes(Type,logprice)) + geom_point()

lm4 <- lm(logprice~Mileage + Liter + Make, data = Cars)
summary(lm4)

lm5 <- lm(logprice~Make+Type+Liter+Mileage, data=Cars)
summary(lm5)

ggplot(Cars, aes(sample = lm5$residuals)) + stat_qq() + stat_qq_line()

ggplot(Cars, aes(x=as.numeric(rownames(Cars)), y=lm5$residuals))+geom_point() +
  geom_line() + geom_hline(aes(yintercept =0))+
  labs(y="Residuals", x="Order")

ggplot(Cars, aes(x=Make, y=lm5$residuals))+geom_point() +
  geom_hline(aes(yintercept = 0))

ggplot(Cars, aes(x=Type, y=lm5$residuals))+geom_point() +
  geom_hline(aes(yintercept = 0))


########## NFL Data #########

NFL <- read.csv("/Users/Aaron/Documents/Applied Statistics/Data/NFL16.csv")

names(NFL)
head(NFL)

pairs(NFL[c(5:7)],pch=16) #columns 5,6,7 are the columns we wanted to look at
lm1 <- lm(WinPct~PointsFor, data=NFL)
summary(lm1)

lm2 <- lm(WinPct~PointsAgainst, data =NFL)
summary(lm2)

lm3 <- lm(WinPct~PointsAgainst+PointsFor, data=NFL)
summary(lm3)

ggplot(NFL, aes(x=lm3$fitted.values, y = lm3$residuals)) +
  geom_point() + geom_hline(aes(yintercept =0)) +
  labs(x= "Fitted Values", y = "Residuals")

ggplot(NFL, aes(sample=lm3$residuals)) + stat_qq() + stat_qq_line() +
  labs(x = "Theoretical Quantiles", y = "Residuals")

summary(lm3)
anova(lm3)
#Model estimates coefficients B0, B1, B2 and variance of error terms sigma_error^2
#Residual Standard Error is Sigma_error
#                 df      SS      MS
# Points For
# Points Against
# Residuals      n-p-1        SSE     MSE=SSE/(n-p-1)
#               p = number of predictors(explanatory variables)
# MS = SS/df
0.27023/29 #residual squared standard error
sqrt(0.27023/29) #
0.41262 + 0.55884 #sum of squares for the model
0.97146/2 # Mean squared
0.48573/0.00932 # F-Stat
# test overall fir of the model w/ANOVA
#Source                    df    SS        MS          F-statistic
#Model(or regression)     p               SSM/p           MSM/MSE
#Error                  n-p-1             SSE/(n-p-1)
#Total                  n-1
# if MSM/MSE = large/small then the model is explaining a lot of the variability in dataset.
#H0 = B1 = B2 = ... = Bp
#HA = At least one Bi != 0
#Recall
#SSTotal = SSModel + SSE
# = SSModel = SSTotal - SSE
# = SSModelfull - SSModelreduced = SSTotalfull - SSEfull-(SSTotalreduced - SSEreduced)
# = SSEreduced - SSEfull
# # of predictors tested is the number of variables that you took out of the model.
reduced = lm(WinPct ~ 1, data=NFL)
anova(reduced, lm3)
pf(52.13, 2, 29, lower.tail = F)

NFL$WinPct100 <- NFL$WinPct*100
lm1 <- lm(WinPct100~PointsFor + PointsAgainst + YardsFor + YardsAgainst + TDs, data
          = NFL)
summary(lm1)
intercept = lm(WinPct100 ~ 1 , data=NFL)
anova(intercept, lm1)

lm2reduced <- lm(WinPct100~PointsAgainst + YardsAgainst, data = NFL)
summary(lm2reduced)
anova(lm2reduced, lm1)

lm3 <- lm(WinPct100~PointsFor + PointsAgainst + YardsAgainst, data = NFL)
summary(lm3)
lm4 <- lm(WinPct100~PointsFor + PointsAgainst + YardsAgainst + TDs, data = NFL)
summary(lm4)

round(cor(NFL[c(6:11)]),digits=3)

####### Butterfly Data ########

Butterfly <- read.csv("/Users/Aaron/Documents/Applied Statistics/ButterBc.csv")
names(Butterfly)
head(Butterfly)

ggplot(Butterfly, aes(x = Temp, y = Wing)) +
  geom_point(aes(color = Sex))
MaleButterflies=subset(Butterfly, Sex=='Male')
FemaleButterflies=subset(Butterfly, Sex=='Female')
mean(MaleButterflies$Wing)
mean(FemaleButterflies$Wing)

lm1 <- lm(Wing~Temp, data = MaleButterflies)
summary(lm1)
lm2 <- lm(Wing~Temp, data = FemaleButterflies)
summary(lm2)


ggplot(Butterfly, aes(x = Temp, y = Wing)) +
  geom_point(aes(color = Sex)) +
  geom_abline(aes(intercept = lm1$coefficients[1], slope = lm1$coefficients[2])) +
  geom_abline(aes(intercept = lm2$coefficients[1], slope = lm2$coefficients[2]))

lm3 <- lm(Wing~Temp + Sex, data = Butterfly)
summary(lm3)

confint(lm3)


######## Day 6 ########

Kids <- read.csv("/Users/Aaron/Documents/Applied Statistics/Data/Kids198.csv")
head(Kids)

ggplot(Kids, aes(x = Age, y = Weight, color = as.factor(Sex))) +
  geom_point() +
  labs(x = "Age (months)", y = "Weight (pounds)") +
  scale_color_discrete(name="Sex",
                       breaks=c("0", "1"),
                       labels=c("Boys", "Girls"))
qplot(Age, Weight, color =as.factor(Sex), data = Kids)

ggplot(Kids, aes(x = Age, y = Weight, color = as.factor(Sex))) +
  geom_point() +
  labs(x = "Age (months)", y = "Weight (pounds)") +
  facet_wrap(~Sex) +
  theme(strip.text.x = element_blank()) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_discrete(name="Sex",
                       breaks=c("0", "1"),
                       labels=c("Boys", "Girls"))
ggplot(Kids, aes(x = Age, y = Weight, color = as.factor(Sex))) +
  geom_point() +
  labs(x = "Age (months)", y = "Weight (pounds)") +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_discrete(name="Sex",
                       breaks=c("0", "1"),
                       labels=c("Boys", "Girls"))

lm6 <- lm(Weight~Age, data = subset(Kids, Sex=="0")) #Boys
coefficients(lm6)

lm7 <- lm(Weight~Age, data = subset(Kids, Sex=="1")) #Girls
coefficients(lm7)

#It appears boys gain weight faster than Girls... Is the difference in 
#slopes statistically significant?

lm8 <- lm(Weight~Age + Sex + Age*Sex, data = Kids)
summary(lm8)
anova(lm8)

#weight = B0 + B1Age + B2Sex + B3 *Age*Sex
#For boys, Sex = 0
#Weight = B0 + B1Age + 0 + 0 which is the same as boys only SLR

#For girls, Sex = 1
#Weight = B0 + B1*Age + B2(1) + B3 * Age(1)
#Weight = (B0 + B2) + (B1+B3)*Age
# (B0 + B2) = girls only intercept and (B1+B3) is the girls only slope.
#B2 is an indication of how much different girls intercept  is than boys.
#B3 is an indication of how much different girls slope is than boys.

#Interaction model... PridectedWeight = -33.693 + 0.909*Age + 31.85057*Sex - 0.28122*Age*Sex
#When Sex = 0 (boys)... PridectedWeight = -33.693 + 0.909*Age
#When Sex = 1 (girls)... PridectedWeight = -33.693 + 0.909*Age + 31.85057 - 0.28122*Age
      # = (-33.693 + 31.85057) + Age * ( 0.909 -  0.28122) this reduces to SLR as well.
#To figure out whether difference in slopes is statistically significant, we can test
# on Ho: B3 = 0 Ha: B3 != 0
# From summary(lm8)
  # t = - 3.445 on Age*Sex coefficient
  # p-value = 0.0007
# Never have only the interaction term alone. Also the interaction between two variables can
# be significant while there is not a significant difference between variables alone.

confint(lm8)
ggplot(Kids, aes(sample = lm8$residuals)) + stat_qq() + stat_qq_line()
ggplot(Kids, aes(x=lm8$fitted.values, y = lm8$res))+
  geom_point() + geom_hline(aes(yintercept = 0))
#Recall: Overall F-Statistic is a measure of how effective "full" model is compared to
#intercept-only model.
#What if we want to test a subset of predictors?
#For this we can use a nested F-test

# Ho:Bi = 0 for all predictors in subset
# Ha:Bi!= 0 at least on coefficient in subset is not 0.
#F-statistic = ((SSModelfull - SSModelreduced)/(# of predictors being tested))/((SSEfull)/(n-p-1))




#Cross-Validation
#Testing our model's predictive ability on new data. Where does the new data come from?
#We can hold back some of our sample data for testing. We split our sample data into 2 pieces:
  # Training Set
  # Testing Set
HousesNY <- read.csv("/Users/Aaron/Documents/Applied Statistics/Data/HousesNY.csv")
names(HousesNY)
head(HousesNY)
Training <- HousesNY[1:35,]
Holdout <- HousesNY[36:53,]
ggplot(Training, aes(x = Size, y = Price)) +
  geom_point() +
  labs(x = "Size (1000 sq. ft.)", y = "Price ($1000)")
lm1 <- lm(Price~Size, data = Training)
summary(lm1)
#how does it look on the training set?
ggplot(Training, aes(x = Size, y = Price)) + geom_point() +
  geom_abline(aes(intercept = lm1$coefficients[1], slope = lm1$coefficients[2]))
#how does it look on the testing set?
ggplot(Holdout, aes(x = Size, y = Price)) + geom_point() +
  geom_abline(aes(intercept = lm1$coefficients[1], slope = lm1$coefficients[2]))
Holdout$PriceHat <- predict(lm1,Holdout)
Holdout$Residuals <- Holdout$Price-Holdout$PriceHat
Holdout[,6:7]
mean(Holdout$Residuals)
SSE <- sum(Holdout$Residuals^2)
SSE
MSE <- SSE/18
MSE
anova(lm1)
#MSE for training data and MSE when calculated for the testing data are similar in size.
crossR <- cor(Holdout$Price,Holdout$PriceHat)
crossRsq <- crossR^2
shrinkage <- summary(lm1)$r.squared-crossRsq
c(crossR,crossRsq,shrinkage)






