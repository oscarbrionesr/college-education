setwd("/Users/oscar/Desktop/Winter2023/STAT469/CollegeEducation")

library(tidyverse)
library(GGally)
library(car)
library(MASS)
library(lmtest)
library(multcomp)

salary <- read_csv("Salary.csv")

#1.----

#scaterplot of GPA and Salary
ggplot(data=salary, mapping=aes(x=GPA, y=Salary)) + geom_point()+geom_smooth(se=FALSE)

#Boxplot of majors and salary
ggplot(data=salary, mapping=aes(x=MajorCategory, y=Salary)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Boxplot of gender and salary
ggplot(data=salary, mapping=aes(x=Gen, y=Salary)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Correlation of GPA and Salary
cor(salary$Salary, salary$GPA)

#3.----

#Model to verify matrices
salary.lm <- lm(formula=Salary~., data=salary)

#model matrix
X <- model.matrix(object=Salary~., data=salary)
y <- salary$Salary

#Coefficients
Bhat <- solve((t(X)%*%X))%*%t(X)%*%y
coef(salary.lm)
Bhat

#standard deviation
S2 <- (t(y-(X%*%Bhat))%*%(y-(X%*%Bhat)))/(756-17-1)
sigma(salary.lm)
sqrt(S2)

#R^2
summary(salary.lm)$r.squared


#4.----

#F-test with anova for gender and major interaction
salaryinter.lm <- lm(formula=Salary~.+Gen:MajorCategory, data=salary)
anova(salaryinter.lm)

#boxplot of salary and gender
ggplot(data=salary, mapping=aes(x=MajorCategory, y=Salary, fill=Gen)) + geom_boxplot()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))


#5.----
#AV Plots
avPlots(salaryinter.lm, ask=FALSE)


#Standardized res
ggplot() + geom_histogram(mapping=aes(x=stdres(salaryinter.lm)))

#KS Test
ks.test(stdres(salaryinter.lm), "pnorm")

#Fitted vals vs. res
ggplot(mapping=aes(x=fitted(salaryinter.lm), y=resid(salaryinter.lm))) + geom_point()

#BP test
bptest(salaryinter.lm)


#6.----
#Confidence intervals of 97%:


#GPA
gpa.lm <- lm(formula=Salary~GPA, data=salary)
confint(gpa.lm,level=.97)

#Gender
gender.lm <- lm(formula=Salary~Gen, data=salary)
confint(gender.lm,level=.97)

#Engineering
engin.lm <- lm(formula=Salary~MajorCategory, data=salary)
confint(engin.lm,level=.97)


#7.----

#Hypothesis test of gender in the field of math and computers
at <- t((c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 4)-c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4)))

my.test <- glht(salary.lm, linfct=at, alternative="two.sided")
summary(my.test)

confint(my.test,level=.95)


#8.----

#Prediction for my personal salary:

new.x = data.frame(MajorCategory="Computers & Mathematics", Gen="M", GPA=3.22)
predict.lm(salary.lm, newdata=new.x, interval="prediction", level=0.95)


#9.---

#Leave one out cross validation:

n.cv <- 756 #Number of CV studies to run
n.test <- 1  #Number of observations in a test set
rpmse <- rep(x=NA, times=n.cv)
wid <- rep(x=NA, times=n.cv)

for(cv in 1:n.cv){
  ## Select test observations
  test.obs <- sample(x=1:n.cv, size=n.test)
  
  ## Split into test and training sets
  test.set <- salary[test.obs,]
  train.set <- salary[-test.obs,]
  
  ## Fit a lm() using the training data
  train.lm <- lm(formula=, data=train.set)
  
  ## Generate predictions for the test set
  my.preds <- predict.lm(train.lm, newdata=test.set, interval="prediction")
  
  ## Calculate RPMSE
  rpmse[cv] <- (test.set[['Salary']]-my.preds[,'fit'])^2 %>% mean() %>% sqrt()

  ## Calculate Width
  wid[cv] <- (my.preds[,'upr'] - my.preds[,'lwr']) %>% mean()
  
}

#RMSE
mean(rpmse)
sigma(salary.lm)

#Width
mean(wid)


