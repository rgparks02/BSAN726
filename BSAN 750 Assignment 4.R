tc=read.csv("TelcoChurn.csv")

View(tc)


### Cleaning the data
tc$gender=as.factor(tc$gender)
tc$Partner=as.factor(tc$Partner)
tc$Dependents=as.factor(tc$Dependents)
tc$Contract=as.factor(tc$Contract)
tc$PaperlessBilling=as.factor(tc$PaperlessBilling)
tc$PaymentMethod=as.factor(tc$PaymentMethod)
tc$Churn=as.factor(tc$Churn)



install.packages("plyr")

library(plyr)

### Changing yes to 1 and no to 0 for the total services column

tc$PhoneService <- revalue(tc$PhoneService, c("Yes"=1))
tc$PhoneService <- revalue(tc$PhoneService, c("No"=0))

tc$MultipleLines <- revalue(tc$MultipleLines, c("Yes"=1))
tc$MultipleLines <- revalue(tc$MultipleLines, c("No"=0))
tc$MultipleLines <- revalue(tc$MultipleLines, c("No phone service"=0))

tc$InternetService <- revalue(tc$InternetService, c("DSL"=1))
tc$InternetService <- revalue(tc$InternetService, c("Fiber optic"=1))
tc$InternetService <- revalue(tc$InternetService, c("No"=0))

tc$OnlineSecurity <- revalue(tc$OnlineSecurity, c("Yes"=1))
tc$OnlineSecurity <- revalue(tc$OnlineSecurity, c("No"=0))
tc$OnlineSecurity <- revalue(tc$OnlineSecurity, c("No internet service"=0))

tc$OnlineBackup <- revalue(tc$OnlineBackup, c("Yes"=1))
tc$OnlineBackup <- revalue(tc$OnlineBackup, c("No"=0))
tc$OnlineBackup <- revalue(tc$OnlineBackup, c("No internet service"=0))

tc$DeviceProtection <- revalue(tc$DeviceProtection, c("Yes"=1))
tc$DeviceProtection <- revalue(tc$DeviceProtection, c("No"=0))
tc$DeviceProtection <- revalue(tc$DeviceProtection, c("No internet service"=0))

tc$StreamingTV <- revalue(tc$StreamingTV, c("Yes"=1))
tc$StreamingTV <- revalue(tc$StreamingTV, c("No"=0))
tc$StreamingTV <- revalue(tc$StreamingTV, c("No internet service"=0))

tc$StreamingMovies <- revalue(tc$StreamingMovies, c("Yes"=1))
tc$StreamingMovies <- revalue(tc$StreamingMovies, c("No"=0))
tc$StreamingMovies <- revalue(tc$StreamingMovies, c("No internet service"=0))

tc$TechSupport <- revalue(tc$TechSupport, c("Yes"=1))
tc$TechSupport <- revalue(tc$TechSupport, c("No"=0))
tc$TechSupport <- revalue(tc$TechSupport, c("No internet service"=0))




### Creating the total services variable

### I couldn't figure this out 

total.services.data= data.frame(tc$PhoneService,tc$MultipleLines,tc$InternetService,tc$OnlineSecurity
                                ,tc$OnlineBackup,tc$DeviceProtection,tc$StreamingTV,
                                  tc$StreamingMovies,tc$TechSupport)

tc$totalservices= rowSums(total.services.data [,c(1:9)])



### Spliting the data

index <- sample(nrow(tc),nrow(tc)*0.80)
train <- tc[index,]
test <- tc[-index,]

### Logistic regression model

library(tidyverse)

tc.model.log.reg=  glm(Churn~. -customerID, family=binomial, data=train)
summary(tc.model.log.reg)

tc.model.log.reg=  glm(Churn~. -customerID -gender - Partner -Dependents - MultipleLines
                       -InternetService -StreamingMovies -StreamingTV, family=binomial, data=train
                       ,na.action=na.omit)
summary(tc.model.log.reg)



### Mean Residual, AIC, BIC

tc.model.log.reg$deviance

AIC(tc.model.log.reg)
BIC(tc.model.log.reg)

pred.tc.log.train<- predict(tc.model.log.reg, type="response")

newdata= na.omit(tc.model.log.reg)

summary(newdata)

### Prediction model

library(ROCR)
pred1 <- prediction(pred.tc.log.train, train$Churn)
perf1 <- performance(pred1, "tpr", "fpr")

### AUC

unlist(slot(performance(pred1, "auc"), "y.values"))


### Stepwise 

library(MASS)

nullmodel<- lm(Churn~1, data=train)
fullmodel<- lm(Churn~.-customerID, data=train)

model.step.tc<- step(nullmodel, scope=list(lower=nullmodel, upper=fullmodel), direction='both')

AIC(model.step.tc)
BIC(model.step.tc)



### Backward

model.back.tc<- step(fullmodel,direction='backward')

AIC(model.back.tc)
BIC(model.back.tc)






