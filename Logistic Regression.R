library(mosaic)
library(dplyr)
library(mosaic)
library(knitr)
library(leaps)

Losing <- read.csv("/Users/Aaron/Documents/Applied Statistics/Data/Losing.csv")
Losing
(SummaryTable <- with(Losing,table(Outcome,Age)))
margin.table(SummaryTable,2)
SleepProp <- prop.table(SummaryTable,2)
round(SleepProp,3)

Age <- 14:18
Prop1 <- SleepProp[2*(1:5)]
SummaryData <- data.frame(Age, Prop1)
SummaryData
ggplot(SummaryData, aes(x = Age, y = Prop1)) +
  geom_point() +
  xlim(12,20) +
  ylim(0.5,0.9) +
  labs(x="Age", y = "Proportion Saying Yes")

lm1 <- lm(Prop1~Age, data=SummaryData)
ggplot(SummaryData, aes(x = Age, y = Prop1)) + geom_point() +
  geom_abline(aes(intercept = lm1$coefficients[1], slope = lm1$coefficients[2]))+
  xlim(0,40) +
  ylim(0,1) +
  labs(x="Age", y = "Proportion Saying Yes")

SummaryData$Odds1=Prop1/(1-Prop1)
SummaryData$logOdds1=log(SummaryData$Odds1)
lm2 <- lm(logOdds1~Age, data=SummaryData)
ggplot(SummaryData, aes(x = Age, y = logOdds1)) + geom_point() +
  geom_abline(aes(intercept = lm2$coefficients[1], slope = lm2$coefficients[2]))+
  xlim(0,40) +
  ylim(-3,3) +
  labs(x="Age", y = "log(Odds of Yes)")

#1. The relationship is one-to-one. That is, for every value of pi (with the two exceptions of 0 and 1), there is one,
#and only one, value of log( pi/(1 − pi)) . This means that the logit transform is reversible.
#2. The log(odds) can take on any value from −∞ to ∞ . This means that we can use a linear predictor of the
#form Thus, the linear logistic model is log(odds) = log / !
#
SummaryData$fitlogodds <- lm2$fitted.values
SummaryData$pihat <- exp(lm2$fitted.values)/(1+exp(lm2$fitted.values))
Round(SummaryData,3)
newdata = data.frame(Age=c(2,12,20,30))
predictlogodds <- predict(lm2, newdata)
predictpihat <- exp(predictlogodds)/(1+exp(predictlogodds))
predictlogodds
predictpihat

MedGPA <- read.csv("/Users/Aaron/Documents/Applied Statistics/Data/MedGPA.csv")
ggplot(MedGPA, aes(x = GPA, y = MCAT)) +
  geom_point() +
  labs(x="GPA", y = "MCAT score")
lm1 <- lm(MCAT~GPA, data=MedGPA)
summary(lm1)
ggplot(MedGPA, aes(x = GPA, y = MCAT)) + geom_point() +
  geom_abline(aes(intercept = lm1$coefficients[1], slope = lm1$coefficients[2]))+
  labs(x="GPA", y = "MCAT score")
ggplot(MedGPA, aes(x = GPA, y = Acceptance)) + geom_point() +
  geom_jitter(height=0.03) +
  labs(x="GPA", y="Acceptance")
logitmod=glm(Acceptance~GPA,family=binomial,data=MedGPA) ##### This is how we build the model!!!
summary(logitmod)
#logit(P(Accepted)) = -19.207 + 5.454*GPA
 #odds(Acceptance1) = e^(log(odd(acceptance)))
  #P(acceptance1) = e^(-19.207 + 5.454*GPA) / (1 + e^(-19.207 + 5.454*GPA))
ggplot(MedGPA, aes(x=GPA, y=Acceptance)) + geom_point() +
  geom_jitter(height=0.03) +
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE) +
  labs(x="GPA", y="P(Accept)")
newdata = data.frame(GPA=3.6)
predictlogodds <- predict(logitmod, newdata)
predictpihat <- exp(predictlogodds)/(1+exp(predictlogodds))
predictlogodds
predictpihat

Migraine <- read.csv("/Users/Aaron/Documents/Applied Statistics/Data/Migraine.csv")
Migraine$PYes <- Migraine$Yes/Migraine$Trials
Migraine$Odds <- Migraine$Yes/Migraine$No
Migraine

PLetrozole <- 0.072
PPlacebo <- 0.132
OddsLetrozole <- PLetrozole/(1-PLetrozole)
OddsLetrozole
OddsPlacebo <- PPlacebo/(1-PPlacebo)
OddsPlacebo

(Migraine$LogOdds=log(Migraine$Odds))
(slope=with(Migraine,LogOdds[1]-LogOdds[2]))

Migraine$GroupCode <- c(1,0)
ggplot(Migraine, aes(x=GroupCode, y=LogOdds)) + geom_point() +
  ylim(-1.5,0.5) +
  scale_x_continuous(name="Treatment 0 = Placebo, 1 = TMS", breaks=c(0, 1)) +
  labs(y="Log Odds of Pain Free") +
  geom_line(linetype = "dashed", color="blue") +
  geom_hline(aes(yintercept=0)) +
  geom_vline(aes(xintercept=0))

lmodTMS <- glm(cbind(Yes,No)~Group,family=binomial,data=Migraine)
summary(lmodTMS)

Putts <- read.csv("/Users/Aaron/Documents/Applied Statistics/Data/Putts.csv")
(Putt2way=table(Putts$Made,Putts$Length))
margin.table(Putt2way,2)
(PuttP=mean(Made~Length,data=Putts))
(OddsMake=PuttP/(1-PuttP))
(LogitP=log(OddsMake))

(LogOddsDiff=LogitP[2:5]-LogitP[1:4])
exp(LogOddsDiff)
(OddsRatios=OddsMake[2:5]/OddsMake[1:4])

lmodPutt=glm(Made~Length,family=binomial,data=Putts)
summary(lmodPutt)
exp(lmodPutt$coeff[2])

logitmod=glm(Acceptance~GPA,family=binomial,data=MedGPA)
summary(logitmod)


MedGPA$GPA10 <- MedGPA$GPA*10
logitmodGPA <- glm(Acceptance~GPA10,family=binomial,data=MedGPA)
summary(logitmodGPA)
confint(logitmodGPA, "GPA10")
exp(confint(logitmodGPA, "GPA10"))

1-pchisq(18.952,1)
logitmodPutt=glm(Made~Length,family=binomial,data=Putts)
summary(logitmodPutt)

(ChisqL0=logitmodPutt$null.deviance)
(ChisqL=logitmodPutt$deviance)
(G=ChisqL0-ChisqL)
(Chisqdf=logitmodPutt$df.null-logitmodPutt$df.residual)
(pvalue=1-pchisq(G,Chisqdf))

logitmodTMS <- glm(cbind(Yes,No)~Group,family=binomial,data=Migraine)
summary(logitmodTMS)
confint(logitmodTMS, "GroupTMS")
exp(confint(logitmodTMS, "GroupTMS"))
predict(logitmodTMS,type="response")

ggplot(MedGPA, aes(x = logitmod$fitted, y = logitmod$res)) +
  geom_point() + geom_hline(aes(yintercept = 0)) +
  labs(x="Fitted Values", y = "Residuals")
ggplot(MedGPA, aes(sample = logitmod$res)) + stat_qq() + stat_qq_line()

lmodPutt=glm(Made~Length,family=binomial,data=Putts)
summary(lmodPutt)
(PuttP=mean(Made~Length,data=Putts))
(OddsMake=PuttP/(1-PuttP))
(LogitP=log(OddsMake))
DataPutts <- data.frame("Length"=3:7, "EmpLogit"=LogitP)
ggplot(DataPutts, aes(x=Length, y=EmpLogit)) +
  geom_point() +
  scale_x_continuous(name="Distance (ft.)", limits=c(2, 8),
                     breaks=seq(2,8,1)) +
  scale_y_continuous(name="Log Odds of Success", limits=c(-2,3),
                     breaks=seq(-2,3,1)) +
  geom_abline(slope=-0.56614, intercept=3.25684, color = "blue", lty=2)

Eyes <- read.csv("/Users/Aaron/Documents/Applied Statistics/Data/Eyes.csv")
ggplot(Eyes, aes(as.factor(Gay), DilateDiff)) + geom_boxplot() + coord_flip()

breaks4 <- quantile(Eyes$DilateDiff,probs=(0:4)/4)
breaks4[1] <- breaks4[1]-1
Eyes$DilateGroup <- cut(Eyes$DilateDiff,breaks=breaks4,labels=1:4)
Group4 <- data.frame(Group=1:4)
Group4$Cases <- tally(~DilateGroup,data=Eyes)
Group4$Mean <- round(mean(DilateDiff~DilateGroup,data=Eyes),3)
Group4$MinDilate <- round(min(DilateDiff~DilateGroup,data=Eyes),3)
Group4$MaxDilate <- round(max(DilateDiff~DilateGroup,data=Eyes),3)
Group4$Gay1 <- sum(Gay~DilateGroup,data=Eyes)
Group4$Prop <- round(Group4$Gay1/Group4$Cases,3)
Group4$AdjProp <- round((Group4$Gay1+0.5)/(Group4$Cases+1),3)
Group4$LogitAdjProp <- round(log(Group4$AdjProp/(1-Group4$AdjProp)),3)
Group4
emplogitmod <- lm(LogitAdjProp~Mean, data=Group4)
summary(emplogitmod)
ggplot(Group4, aes(x=Mean, y=as.numeric(LogitAdjProp))) + geom_point() +
  labs(x="Mean Dilation Difference",y="Log(Odds of Gay)") +
  geom_abline(slope=2.17394, intercept=-0.51990, color = "blue")
Group4Both <- data.frame(GroupF=1:4,GroupM=1:4)
Group4Both[,3:4] <- tally(~DilateGroup+Sex,data=Eyes)
Group4Both[,5:6] <- round(mean(DilateDiff~DilateGroup+Sex,data=Eyes),3)
Group4Both[,7:8] <- sum(Gay~DilateGroup+Sex,data=Eyes)
Group4Both[,9:10] <- round(Group4Both[,7:8]/Group4Both[,3:4],3)
Group4Both[,11:12] <- round((Group4Both[,7:8]+0.5)/(Group4Both[,3:4]+1),3)
Group4Both[,13:14] <- round(log(Group4Both[,11:12]/(1-Group4Both[,11:12])),3)
names(Group4Both)[3:14] <-
  c("FCases","MCases","FMean","MMean","FCount","MCount","FProb","MProb","FAdjProb","MA
    djProb","FLogitAdjProb","MLogitAdjProb")
Group4Both
emplogitmodF <- lm(FLogitAdjProb~FMean, data=Group4Both)
summary(emplogitmodF)
emplogitmodM <- lm(MLogitAdjProb~MMean, data=Group4Both)
summary(emplogitmodM)
ggplot() +
  geom_point(data=Group4Both, aes(x=FMean, y=as.numeric(FLogitAdjProb)),
             color="purple") +
  geom_point(data=Group4Both, aes(x=MMean, y=as.numeric(MLogitAdjProb)),
             color="blue") +
  scale_y_continuous(name="Log(Odds of Gay)", limits=c(-3,1), breaks=seq(-3,1,1)) +
  geom_abline(slope=2.8106, intercept=-1.1142, color = "purple") +
  geom_abline(slope=1.63391, intercept=-0.04638, color = "blue") +
  xlab("Mean Dilation Difference")
logitmodEyes=glm(Gay~DilateDiff+Sex,family=binomial,data=Eyes)
summary(logitmodEyes)

curve(predict(logitmodEyes,data.frame(DilateDiff=x,Sex="F"),type="response"),xlim=c(
  -1,1),ylim=c(0,1),col="purple",lwd=2,xlab="DialateDiff",ylab="P(Gay)")
curve(predict(logitmodEyes,data.frame(DilateDiff=x,Sex="M"),type="response"),lty=2,col="blue",lwd=2,add=TRUE)
text(-0.6,0.3,"Males",col="blue")
text(0.6,0.5,"Females",col="purple")


curve(predict(logitmodEyes,data.frame(DilateDiff=x,Sex="F")),xlim=c(-1,1),ylim=c(-
                                                                                   3,3),col="purple",lwd=2,xlab="DialateDiff",ylab="logit(P(Gay)")
curve(predict(logitmodEyes,data.frame(DilateDiff=x,Sex="M")),lty=2,col="blue",lwd=2,
      add=TRUE)
text(-0.6,-1,"Males",col="blue")
text(0.6,0,"Females",col="purple")
(logitpihatM=as.numeric(logitmodEyes$coeff[1]+logitmodEyes$coeff[2]*0.5+logitmodEyes
                        $coeff[3]*1))
(pihatM=exp(logitpihatM)/(1+exp(logitpihatM)))
predict(logitmodEyes,data.frame(DilateDiff=0.5,Sex="M"))
predict(logitmodEyes,data.frame(DilateDiff=0.5,Sex="M"),type="response")
predict(logitmodEyes,data.frame(DilateDiff=0.5,Sex="F"))
predict(logitmodEyes,data.frame(DilateDiff=0.5,Sex="F"),type="response")

logitmodGPA1=glm(Acceptance~MCAT,family=binomial,data=MedGPA)
summary(logitmodGPA1)
logitmodGPA2=glm(Acceptance~MCAT+GPA,family=binomial,data=MedGPA)
summary(logitmodGPA2)
logitmodGPA3=glm(Acceptance~MCAT+GPA+Apps,family=binomial,data=MedGPA)
summary(logitmodGPA3)





  