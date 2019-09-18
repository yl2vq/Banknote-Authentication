#final project#
setwd("C:\\Users\\lyh51\\Desktop\\Desktop\\UVA\\2019 Spring\\STAT 5330\\Final")
banknote<-read.table(file="data_banknote_authentication.txt",sep=",")
names(banknote) <- c("variance", 
                     "skewness", 
                     "curtosis",
                     "entropy",
                     "class")
banknote <- banknote %>%
  select(-("variance"))
pairs(banknote, gap=0, pch='.')

summary(banknote)

library(ISLR)
attach(banknote)
sampsize = floor(0.75*nrow(banknote))
set.seed(2)
train_ind = sample(seq_len(nrow(banknote)),size = sampsize)  
train =banknote[train_ind,] 
test=banknote[-train_ind,]

## Random Sampling

svmacc = vector()
svmsensi = vector()
svmspec = vector()

rfacc = vector()
rfsensi = vector()
rfspec = vector()
for (i in seq(1:20)){
  sampsize = floor(0.75*nrow(banknote))
  set.seed(i)
  train_ind = sample(seq_len(nrow(banknote)),size = sampsize)  
  train =banknote[train_ind,] 
  test=banknote[-train_ind,]
  svmfit.linear = svm(class ~ ., data = train, type='C-classification', kernel = "radial", cost = 1, scale = FALSE)
  svm.pred.linear <- predict(svmfit.linear, test)
  svm.table.linear <- table(svm.pred.linear, test$class)
  svm.accuracy.linear <- (svm.table.linear["0","0"] + svm.table.linear["1","1"])/n
  svm.sensitivity.linear <- svm.table.linear["1","1"]/(svm.table.linear["0","1"] + svm.table.linear["1","1"])
  svm.specificity.linear <- svm.table.linear["0","0"]/(svm.table.linear["1","0"] + svm.table.linear["0","0"])
  svmacc <- c(svmacc, svm.accuracy.linear)
  svmsensi <- c(svmsensi, svm.sensitivity.linear)
  svmspec <- c(svmspec, svm.specificity.linear)
  
  control <- trainControl(method="cv", number=5)
  rf.fit = randomForest(as.factor(class)~.,data=train, ntree = 500, mtry = 2,nodedize = 1,importance=TRUE, trControl = control)
  rf.predictions <- predict(rf.fit, test)
  rf.table <- table(rf.predictions,test$class)
  
  rf.accuracy <- (rf.table["0","0"] + rf.table["1","1"])/n
  rf.sensitivity <- rf.table["1","1"]/(rf.table["0","1"] + rf.table["1","1"])
  rf.specificity <- rf.table["0","0"]/(rf.table["1","0"] + rf.table["0","0"])
  rfacc <- c(rfacc, rf.accuracy)
  rfsensi <- c(rfsensi, rf.sensitivity)
  rfspec <- c(rfspec, rf.specificity)
  
}

svmacc 
svmsensi 
svmspec 
rfacc
rfsensi
rfspec 

accdf <- data.frame(x=seq(1:20),svmacc,rfacc)
sensidf <- data.frame(x=seq(1:20),svmsensi,rfsensi)
specdf <- data.frame(x=seq(1:20),svmspec,rfspec)

ggplot(accdf) + geom_line(aes(x=x,y=svmacc),col='Blue') + geom_line(aes(x=x,y=rfacc),col='red')
ggplot(sensidf) + geom_line(aes(x=x,y=svmsensi),col='Blue') + geom_line(aes(x=x,y=rfsensi),col='red')
ggplot(specdf) + geom_line(aes(x=x,y=svmspec),col='Blue') + geom_line(aes(x=x,y=rfspec),col='red')

############################################################################
library(MASS)
library(e1071)
library(caret)
library(party)
library(rpart)
library(tree)
library(randomForest)
library(caret)
library(tidyverse)
# Descriptive Analysis 

## proportion of real bankote
proportion = sum(banknote$class == 1)/nrow(banknote)
table(banknote$class)/nrow(banknote)
n=nrow(test)
## missing data?
apply(train, 2, function(x) sum(is.na(x)))
## summary of variables
apply(train,2,summary)

# Training Model 

## Logistic Regression Model 
logitMod <- glm(class~., data=train, family=binomial(link="logit"))
predicted <- predict(logitMod, test, type="response")
logit.table <- table(ifelse(predicted>0.4, "1","0"), test$class)
logit.accuracy = (logit.table["0","0"] + logit.table["1","1"])/n
logit.sensitivity = logit.table["1","1"]/(logit.table["0","1"] + logit.table["1","1"])
logit.specificity = logit.table["0","0"]/(logit.table["1","0"] + logit.table["0","0"])

## lda (total)
lda.model.total = lda(class~., data=train)
lda.pred.total = predict(lda.model.total, test)
lda.table.total = table(Predicted = lda.pred.total$class, Class=test$class)

lda.accuracy.total = (lda.table.total["0","0"] + lda.table.total["1","1"])/n
lda.sensitivity.total = lda.table.total["1","1"]/(lda.table.total["0","1"] + lda.table.total["1","1"])
lda.specificity.total = lda.table.total["0","0"]/(lda.table.total["1","0"] + lda.table.total["0","0"])

## qda(total)

qda.model.total = qda(class~., data=train)
qda.pred.total = predict(qda.model.total, test)
qda.table.total = table(Predicted = qda.pred.total$class, Class=test$class)
qda.table.total
qda.accuracy.total = (qda.table.total["0","0"] + qda.table.total["1","1"])/n
qda.sensitivity.total = qda.table.total["1","1"]/(qda.table.total["0","1"] + qda.table.total["1","1"])
qda.specificity.total = qda.table.total["0","0"]/(qda.table.total["1","0"] + qda.table.total["0","0"])


## svm 
svmfit.linear = svm(class ~ ., data = train, type='C-classification', kernel = "radial", cost = 1, scale = FALSE)
svm.pred.linear <- predict(svmfit.linear, test)
svm.table.linear <- table(svm.pred.linear, test$class)
svm.accuracy.linear <- (svm.table.linear["0","0"] + svm.table.linear["1","1"])/n
svm.sensitivity.linear <- svm.table.linear["1","1"]/(svm.table.linear["0","1"] + svm.table.linear["1","1"])
svm.specificity.linear <- svm.table.linear["0","0"]/(svm.table.linear["1","0"] + svm.table.linear["0","0"])


### node size = 1
control <- trainControl(method="cv", number=5)
rf.fit = randomForest(as.factor(class)~.,data=train, ntree = 500, mtry = 2,nodedize = 1,importance=TRUE, trControl = control)
rf.predictions <- predict(rf.fit, test)
rf.table <- table(rf.predictions,test$class)

rf.accuracy <- (rf.table["0","0"] + rf.table["1","1"])/n
rf.sensitivity <- rf.table["1","1"]/(rf.table["0","1"] + rf.table["1","1"])
rf.specificity <- rf.table["0","0"]/(rf.table["1","0"] + rf.table["0","0"])

# g

varImpPlot(rf.fit)
barplot(importance(rf.fit)[,4])

# h

library(gbm)

gbmGrid <-  expand.grid( 
  n.trees = c(100,200,500,1000,2000), 
  shrinkage = seq(0.01,0.1,0.01),
  interaction.depth = 1, n.minobsinnode = 10)

nrow(gbmGrid)

gbmfit <- train(as.factor(class) ~ ., data = train, distribution="adaboost", method = "gbm",  trControl = control, tuneGrid = gbmGrid, verbose=FALSE)

gbmpred = predict(gbmfit, test)
gbm.table <- table(gbmpred,test$class)
gbm.accuracy = (gbm.table["0","0"] + gbm.table["1","1"])/n
gbm.sensitivity = gbm.table["1","1"]/(gbm.table["0","1"] + gbm.table["1","1"])
gbm.specificity = gbm.table["0","0"]/(gbm.table["1","0"] + gbm.table["0","0"])

train_control<- trainControl(method="cv", number=5, savePredictions = TRUE)

###########################################################################

# Logistic Regression
log_model <- glm(train$class ~., family = binomial, data = train) 
summary(log_model)

result_log <- predict(log_model,newdata=test,type='response')
result_log2 <- ifelse(result_log > 0.4,1,0)
log_tab <- table(result_log2, test$class)

confusionMatrix(log_tab)


# Support Vector Machine
library(e1071)
svm_model <- svm(train$class ~., data = train, kernel = "radial", cost = 10, scale = FALSE, probability = TRUE)
summary(svm_model)

result_svm <- predict(svm_model, type="prob", newdata=test, probability = TRUE)
svm_tab <- table(result_svm, test$class)
confusionMatrix(svm_tab)

svmfit.linear = svm(class ~ ., data = train, type='C-classification', kernel = "linear", cost = 1, gamma=0.01, scale = FALSE)
svm.pred.linear <- predict(svmfit.linear, test)
svm.table.linear <- table(svm.pred.linear, test$class)
svm.accuracy.linear <- (svm.table.linear["0","0"] + svm.table.linear["1","1"])/n
svm.sensitivity.linear <- svm.table.linear["1","1"]/(svm.table.linear["0","1"] + svm.table.linear["1","1"])
svm.specificity.linear <- svm.table.linear["0","0"]/(svm.table.linear["1","0"] + svm.table.linear["0","0"])

# Random Forest
library(randomForest)
rf_model <- randomForest(as.factor(train$class) ~ ., data=train, ntree=100, mtry=4, importance=TRUE)

result_rf <- predict(rf_model,newdata=test, type="prob")
result_rf2 <- predict(rf_model,newdata=test)
rf_tab <- table(result_rf2, test$class)
confusionMatrix(rf_tab)

### setting for ROC

library(ROCR) 

log_fitpred = prediction(result_log, test$class)
log_fitperf = performance(log_fitpred,"tpr","fpr") 

svm_fitpred <- prediction(attr(result_svm, "probabilities")[,2], test$class)
svm_fitpref <- performance(svm_fitpred,"tpr","fpr") 

rf_fitpred <- prediction(result_rf[,2], test$class)
rf_fitpref <- performance(rf_fitpred,"tpr","fpr") 


# plot ROC curve
plot(log_fitperf,col="red",lwd=1,main="ROC Curve")

# add 2 more 
plot(svm_fitpref, add=TRUE, col="blue") 
plot(rf_fitpref, add=TRUE, col='blue')

# add baseline
abline(a=0,b=1,lwd=2,lty=2,col="gray")


library(rpart.plot)
cforest(class ~ ., data=train, controls=cforest_control(mtry=2, mincriterion=0))
tree1 <- rpart(formula = class~., 
               data = train, 
               method = 'class')
invisible(rpart.plot(tree1))

predicted <- predict(logitMod, test, type="response")
logit.predict = predicted 
svm.pred.linear <- predict(svmfit.linear, test)
svm.predict = svm.pred.linear
result_rf <- predict(rf_model,newdata=test, type="prob")
rf.predict = result_rf

install.packages("caTools")
library(caTools)

colAUC(cbind(predicted, svm.pred.linear, rf.predictions,gbmpred), test$class, plotROC = T)

install.packages("kernlab")
library(kernlab)

variance = test$variance
skewness = test$skewness
curtosis = test$curtosis
entropy = test$entropy
class = test$class
d = data.frame(class = class, variance=variance, skewness=skewness, curtosis=curtosis, entropy=entropy)
model.ksvm = ksvm(class~., data = test, type="C-svc")
plot(model.ksvm, data=test)
plot(svm.pred.linear, data=train)

iris.part = subset(banknote, class != 1)
iris.part$class = factor(iris.part$class)
iris.part = iris.part[, c(1,2,5)]
fit = svm(class ~ ., data=banknote, type='C-classification', kernel='linear')
plot(fit, iris.part)
