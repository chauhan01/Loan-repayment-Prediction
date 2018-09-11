

#----------------------logistic regrssion -----------------------------------#

#Problem statement : prediction loan repayment



#---------------preparing enviroment for logistic enviroment-----------------------

library(caret)
library(ggplot2)
library(MASS)
library(car)
library(mlogit)
library(sqldf)
library(Hmisc)
library(aod)
library(caTools)
library(BaylorEdPsych)
library(pROC)
library(ROCR)
library(ResourceSelection)


#setting up working directory

getwd()
path <- "C:/Users/shubham/Desktop/Logistic regression case study 2"
setwd(path)


# reading csv file

data <- read.csv("loans.csv")
data1<- data # creating backup

str(data1)
dim(data1)

# converting variables to factor

data1$credit.policy <- as.factor(data1$credit.policy)

data1$not.fully.paid <- as.factor(data1$not.fully.paid)

lapply(data1, levels) #checking levels


#-----------------------Missing values----------------------#

as.data.frame(colSums(is.na(data1)))

##---->Substituting missing values with mean

data1[is.na(data1$log.annual.inc),5]=mean(data1$log.annual.inc,na.rm=T)
data1[is.na(data1$days.with.cr.line),8]=mean(data1$days.with.cr.line,na.rm=T)
data1[is.na(data1$revol.util),10]=mean(data1$revol.bal,na.rm=T)


#removing rows with missing values in 3 columns
data2<-data1[complete.cases(data1),]

data.frame(colSums(is.na(data2)))
nrow(data1)-nrow(data2) # 29 rows removed


#renaming dependent variable

colnames(data2) [which(names(data2) == "not.fully.paid")] <- "notPaid"
colnames(data2) [which(names(data2) == "credit.policy")] <- "credit_policy"
colnames(data2) [which(names(data2) == "inq.last.6mths")] <- "inq_Last_6mths"
colnames(data2) [which(names(data2) == "delinq.2yrs")] <- "delinq_2yrs"
colnames(data2) [which(names(data2) == "pub.rec")] <- "pub_rec"



#--------------------------------Information Value Calculation (A variable reduction technique)----------------------------------#

#-----------> Creating two data sets for numeric and categorical values

## Data set with numeric variable
num <- data2[,-c(1,2,11:13)]#Numerical Data Frame
cat <- data2[,c(1,2,11:14)]#Categorical Data Frame
head(cat)
head(num)
str(num)
str(cat)


#---------------------------------------IV for numeric data-------------------------------------------------------#


IVCal <- function(variable, target,data,groups)
{
  data[,"rank"] <- cut2(data[,variable],g=groups)
  tableOutput <-sqldf(sprintf("select rank, 
                              count(%s) n,
                              sum(%s) good
                              from data 
                              group by rank",target,target))
  tableOutput <- sqldf("select *,
                       (n - good) bad
                       from tableOutput")
  tableOutput$bad_rate<- tableOutput$bad/sum(tableOutput$bad)*100
  tableOutput$good_rate<- tableOutput$good/sum(tableOutput$good)*100
  tableOutput$WOE<- (log(tableOutput$good_rate/tableOutput$bad_rate))*100
  tableOutput$IV <- (log(tableOutput$good_rate/tableOutput$bad_rate))*(tableOutput$good_rate-tableOutput$bad_rate)/100
  IV <- sum(tableOutput$IV[is.finite(tableOutput$IV)])
  IV1 <- data.frame(cbind(variable,IV))
  return(IV1)
}

a1<- IVCal("int.rate","notPaid",num,groups=10)
a2<- IVCal("installment","notPaid",num,groups=10)
a3<- IVCal("log.annual.inc","notPaid",num,groups=10)
a4<- IVCal("dti","notPaid",num,groups=10)
a5<- IVCal("fico","notPaid",num,groups=10)
a6<- IVCal("days.with.cr.line","notPaid",num,groups=10)
a7<- IVCal("revol.bal","notPaid",num,groups=10)
a8<- IVCal("revol.util","notPaid",num,groups=10)


IV_num<- data.frame(rbind(a1,a2,a3,a4,a5,a6,a7,a8))
IV_num



#-------------------------------------Information Value for categorical data----------------------------------------------------------#

CA <- function(target, variable, data) {
  A1<- fn$sqldf("select $variable,count($target)n, sum($target)good from data group by $variable")
  
  A1<- fn$sqldf("select *, (n-good) bad from A1")
  A1$bad_rate <- A1$bad/sum(A1$bad)*100
  
  A1$good_rate<- A1$good/sum(A1$good)*100
  A1$WOE<- (log(A1$good_rate/A1$bad_rate))*100
  A1$IV <- (log(A1$good_rate/A1$bad_rate))*(A1$good_rate-A1$bad_rate)/100
  IV <- sum(A1$IV[is.finite(A1$IV)])
  IV1 <- data.frame(cbind(variable,IV))
  return(IV1)
}
A<- CA("notPaid","credit_policy",cat)
B<- CA("notPaid","purpose",cat)
C<- CA("notPaid","inq_last_6mths",cat)
D<- CA("notPaid","delinq_2yrs",cat)
E<- CA("notPaid","pub_rec",cat)



IV_cat<- data.frame(rbind(A,B,C,D,E))
IV_cat
Final_IV <- data.frame(rbind(IV_num,IV_cat))
Final_IV

write.csv(Final_IV,"Final_IV.csv")


########################################################### IV Ends here ##############################################

#--------------------------Splitting the data into training and test data set------------------------#



spl <- sample.split(data2$notPaid, 0.7)

train <- subset(data2, spl == T)

test <- subset(data2, spl == F)

str(train)
str(test)
summary(train)
summary(test)

dim(train)
dim(test)


#-------------------------------------Logistic Regression Model Building------------------------------------------#


model <- glm(notPaid~., train, family=binomial())
View(model)
summary(model)


#Iteration 1
model <- glm(notPaid~.- int.rate, train, family=binomial())
summary(model)

#Iteration 2
model <- glm(notPaid~.- int.rate -dti, train, family=binomial())
summary(model)


#Iteration 3
model <- glm(notPaid~.- int.rate -dti - days.with.cr.line,  train , family=binomial())
summary(model)


#Iteration 4
model <- glm(notPaid~.- int.rate -dti - days.with.cr.line -revol.util,  train , family=binomial())
summary(model)


#Iteration 5
model <- glm(notPaid~ credit_policy + I(purpose == "credit_card")+I(purpose == "debt_consolidation")+I(purpose == "major_purchase") +I(purpose == "small_business")
             + installment + log.annual.inc + fico + revol.bal + inq_Last_6mths + pub_rec,  train , family=binomial())
summary(model)

vif(model)



#--------------->using Wald Test
wald.test(b=coef(model), Sigma= vcov(model), Terms=1:10)
#Since, p-value is less then 0.001, hence we reject Ho that the all Bi=0


# Difference betweene null deviance and deviance
modelChi <- model$null.deviance - model$deviance
modelChi

#Finding the degree of freedom for Null model and model with variables
chidf <- model$df.null - model$df.residual
chidf



# If p value is less than .05 then we reject the null hypothesis that the model is no better than chance.
chisq.prob <- 1 - pchisq(modelChi, chidf)
format(round(chisq.prob, 2), nsmall = 5)

options(scipen = 999)

PseudoR2(model)

# Hosmer and Lemeshow given by the McFadden 6R square
R2.hl<-modelChi/model$null.deviance
R2.hl


# Cox and Snell R Square (the last number; 

R.cs <- 1 - exp ((model$deviance - model$null.deviance) /nrow(data.train))
R.cs

# Max rescaled R square (Nagelkarke) 

R.n <- R.cs /(1-(exp(-(model$null.deviance/(nrow(data.train))))))
R.n



#--------------------Lackfit Deviance for assessing wether the model where
#Ho: Observed Frequencies/probabilties =Expected FRequencies/probabilties ----------------------------------------#
residuals(model) # deviance residuals
residuals(model, "pearson") # pearson residuals

sum(residuals(model, type = "pearson")^2)
deviance(model)

#########Larger p value indicate good model fit
1-pchisq(deviance(model), df.residual(model))
#Thus, we accept the Null Hypthesis Ho thet Observed Frequencies = Expected Frequencies




# Hosmer and Lemeshow test 


hl <- hoslem.test(as.integer(train$notPaid), fitted(model), g=10)
hl


#####################################################################################################################
# Coefficients (Odds)
model$coefficients
# Coefficients (Odds Ratio)
exp(model$coefficients)


# Variable Importance of the model
varImp(model)

# Predicted Probabilities
prediction <- predict(model,newdata = train,type="response")
prediction

write.csv(prediction,"pred.csv")


rocCurve   <- roc(response = train$notPaid, predictor = prediction, 
                  levels = rev(levels(train$notPaid)))
train$notPaid <- as.factor(train$notPaid)

#Metrics - Fit Statistics

predclass <-ifelse(prediction>coords(rocCurve,"best")[1],1,0)
Confusion <- table(Predicted = predclass,Actual = train$notPaid)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
Gini <-2*auc(rocCurve)-1

AUCmetric <- data.frame(c(coords(rocCurve,"best"),AUC=auc(rocCurve),AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
AUCmetric

Confusion 
plot(rocCurve)



# KS statistics calculation

train$m1.yhat <- predict(model, train, type = "response")
m1.scores <- prediction(train$m1.yhat, train$notPaid)

plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")

m1.perf <- performance(m1.scores, "tpr", "fpr")
ks1.logit <- max(attr(m1.perf, "y.values")[[1]] - (attr(m1.perf, "x.values")[[1]]))
ks1.logit # Thumb rule : should lie between 0.4 - 0.7



###################### Residual Analysis ################################################################################


logistic_data <-train

logistic_data$predicted.probabilities<-fitted(model)

logistic_data$standardized.residuals<-rstandard(model)
logistic_data$studentized.residuals<-rstudent(model)
logistic_data$dfbeta<-dfbeta(model)
logistic_data$dffit<-dffits(model)
logistic_data$leverage<-hatvalues(model)

logistic_data[, c("leverage", "studentized.residuals", "dfbeta")]
write.csv(logistic_data, "Res.csv")

# Testing model on test dataset

fit <- glm(notPaid~ credit_policy + I(purpose == "credit_card")+I(purpose == "debt_consolidation")+I(purpose == "major_purchase") +I(purpose == "small_business")
             + installment + log.annual.inc + fico + revol.bal + inq_Last_6mths + pub_rec,  train , family=binomial())
summary(fit)




vif(fit)

#--------------->using Wald Test
wald.test(b=coef(fit), Sigma= vcov(fit), Terms=1:10)
#Since, p-value is less then 0.001, hence we reject Ho that the all Bi=0


# Difference between -2LL of Null model and model with variables
modelChi <- fit$null.deviance - fit$deviance
modelChi

#Finding the degree of freedom for Null model and model with variables
chidf <- fit$df.null - fit$df.residual
chidf

# With more decimal places
# If p value is less than .05 then we reject the null hypothesis that the model is no better than chance.
chisq.prob <- 1 - pchisq(modelChi, chidf)
format(round(chisq.prob, 2), nsmall = 5)


# Hosmer and Lemeshow R square
R2.hl<-modelChi/fit$null.deviance
R2.hl


# Cox and Snell R Square (the last number; here is 2000 should be total no. of ovservation)

R.cs <- 1 - exp ((fit$deviance - fit$null.deviance) /2000)
R.cs

# Max rescaled R square (Nagelkarke) (the last number; here is 2000 should be total no. of ovservation)

R.n <- R.cs /(1-(exp(-(fit$null.deviance/2000))))
R.n



######### Lackfit Deviance ######################################################
residuals(fit) # deviance residuals
residuals(fit, "pearson") # pearson residuals

sum(residuals(fit, type = "pearson")^2)
deviance(fit)

#########Large p value indicate good model fit
1-pchisq(deviance(fit), df.residual(fit))



# Hosmer and Lemeshow test 
## High p value incidates the model fits well

library(ResourceSelection)
hl <- hoslem.test(test$notPaid, fitted(fit), g=10)
hl

# Coefficients (Odds)
fit$coefficients

# Coefficients (Odds Ratio)
exp(fit$coefficients)

# Predicted Probabilities
prediction <- predict(fit,newdata = test,type="response")
prediction



rocCurve   <- roc(response = test$notPaid, predictor = prediction, 
                  levels = rev(levels(test$notPaid)))
test$notPaid <- as.factor(test$notPaid)


#Metrics - Fit Statistics

predclass <-ifelse(prediction>coords(rocCurve,"best")[1],1,0)
Confusion <- table(Predicted = predclass,Actual = test$notPaid)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
Gini <-2*auc(rocCurve)-1

AUCmetric <- data.frame(c(coords(rocCurve,"best"),AUC=auc(rocCurve),AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
AUCmetric

Confusion 
plot(rocCurve)



## KS statistics calculation
test$m1.yhat <- predict(fit, test, type = "response")

library(ROCR)
m1.scores <- prediction(test$m1.yhat, test$notPaid)

plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")

m1.perf <- performance(m1.scores, "tpr", "fpr")
ks1.logit <- max(attr(m1.perf, "y.values")[[1]] - (attr(m1.perf, "x.values")[[1]]))
ks1.logit # Thumb rule : should lie between 40 - 70



###################### Residual Analysis ################################################################################


logistic_data <- test

logistic_data$predicted.probabilities<-fitted(fit)
logistic_data$standardized.residuals<-rstandard(fit)
logistic_data$studentized.residuals<-rstudent(fit)
logistic_data$dfbeta<-dfbeta(fit)
logistic_data$dffit<-dffits(fit)
logistic_data$leverage<-hatvalues(fit)

