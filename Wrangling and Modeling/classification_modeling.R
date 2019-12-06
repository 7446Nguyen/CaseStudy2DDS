options(scipen=999)
options(max.print=100000)

library(pacman)
p_load(tidyverse, caret, ROCR, Metrics, glmnet, MASS, e1071, car, class, mlbench)


#Model Selection###############################################################################################################
###############################################################################################################################
### Initial Models
model.forward.Start <- glm(Attrition ~ 1, family=binomial(link='logit'), data = caseDataCleanLogM)
model.Allvar <- glm(Attrition ~ ., family=binomial(link='logit'), data = caseDataCleanLogM)
########################################################################

#### Forward Selection
model.Forward <- stepAIC(model.forward.Start, direction = "forward", trace = F, scope = formula(model.Allvar))

summary(model.Forward)
model.Forward$anova
#################################### Forward Selection Model Suggestion
forward.glm <- glm(Attrition ~ OverTime + StockOptionLevel + JobRole + JobInvolvement + 
                     JobLevel + DistanceFromHome + EnvironmentSatisfaction + JobSatisfaction + 
                     NumCompaniesWorked + satisfaction + YearsSinceLastPromotion + 
                     totalYears + BusinessTravel + overtimeRate + TrainingTimesLastYear + 
                     Age + EmployeeNumber + MaritalStatus + DailyRate
                   , family=binomial(link='logit')
                   , data=caseDataCleanLogM)

summary(forward.glm)
########################################################################

# Backward Elimination
model.Backward <- stepAIC(model.Allvar, direction = "backward", trace = F, scope = formula(model.forward.Start))
summary(model.Backward)
model.Backward$anova
#################################### Backward Elimination Model Suggestion
back.glm <- glm(Attrition ~ Age + BusinessTravel + DailyRate + 
                  DistanceFromHome + EmployeeNumber + EnvironmentSatisfaction + 
                  JobInvolvement + JobLevel + JobRole + JobSatisfaction + MaritalStatus + 
                  NumCompaniesWorked + OverTime + RelationshipSatisfaction + 
                  StockOptionLevel + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  totalYears + overtimeRate
                , family=binomial(link='logit')
                , data=caseDataCleanLogM)

summary(back.glm)
back.glm$aic
########################################################################


# Stepwise Regression
model.Stepwise <- stepAIC(model.Allvar, direction = "both", trace = F)
summary(model.Stepwise)
model.Stepwise$anova

#################################### Stepwise Regression Model Suggestion
step.glm <- glm(Attrition ~ Age + BusinessTravel + DailyRate + 
                  DistanceFromHome + EmployeeNumber + EnvironmentSatisfaction + 
                  JobInvolvement + JobLevel + JobRole + JobSatisfaction + MaritalStatus + 
                  NumCompaniesWorked + OverTime + RelationshipSatisfaction + 
                  StockOptionLevel + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  totalYears + overtimeRate
                , family=binomial(link='logit')
                , data=caseDataCleanLogM)

summary(step.glm)
step.glm$aic
vif(step.glm)

# Model Selection Statistics AIC and RMSE
model_stats <- data.frame(cbind(rbind("Forwards","Backwards","Stepwise"),rbind(round(forward.glm$aic, digits=2),round(back.glm$aic, digits=2),round(step.glm$aic, digits=2)),rbind(round(forward.glm$deviance, digits=2),round(back.glm$deviance, digits=2),round(step.glm$deviance, digits=2))))
colnames(model_stats) <- c("Selection Type", "AIC", "Residual Deviance")

kable(model_stats,format="latex", booktabs = T) %>%
  kable_styling(latex_options="striped", position = "center")

#Cross Validation #############################################################################################################
###############################################################################################################################
#K-fold CV
set.seed(100)
Train <- createDataPartition(caseDataCleanLogM$Attrition, p=0.75, list=FALSE)
training <- caseDataCleanLogM[ Train, ]
testing <- caseDataCleanLogM[ -Train, ]

#train for specficity??? option
ctrl <- trainControl(method = "repeatedcv",
                     number = 25,
                     repeats = 10,
                     classProbs = T)

#combined shot type used instead of action type - test set has action types that are not in the training set

mod_fit <- train(Attrition ~ OverTime + StockOptionLevel + JobRole + JobInvolvement + 
                   JobLevel + DistanceFromHome + EnvironmentSatisfaction + JobSatisfaction + 
                   NumCompaniesWorked + satisfaction + YearsSinceLastPromotion + 
                   totalYears + BusinessTravel + overtimeRate + TrainingTimesLastYear + 
                   Age + EmployeeNumber + MaritalStatus + DailyRate,
                 data=training, 
                 method="glm", 
                 family="binomial", 
                 trControl = ctrl, 
                 tuneLength = 5, 
                 metric = "Specificity")


#Logistic Model Evaluation #####################################################################################################
#Internal Model Performance Metrics
#confusion matrix https://rpubs.com/dvorakt/255527

pred = predict(mod_fit, newdata=testing)
cf = confusionMatrix(table(data=as.numeric(pred>0.5), testing$Attrition))
misclassificationRateInternal = (cf$table[2,1]+cf$table[1,2]) / sum(cf$table)



#ROC/AUC
# Compute AUC for predicting Class with the model
pred <- prediction(pred, testing$Attrition)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")

aucInternal <- performance(pred, measure = "auc")
aucInternal <- aucInternal@y.values[[1]]
#Parameter tunning
plot(perf)
plot(perf, colorize=TRUE)
plot(perf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7), ylab="Sensitivity (TP Rate)",xlab="Specificty (TN Rate)", main = "Internal CV - ROC Curve")

########################################################
#External Model performance Metrics
ex.pred = predict(mod_fit, newdata=caseDataCleanLogM)
ex.cf = confusionMatrix(table(data=as.numeric(ex.pred>0.40), caseDataCleanLogM$Attrition))
misclassificationRateEx = (cf$table[2,1]+cf$table[1,2]) / sum(cf$table)

#ROC/AUC
# Compute AUC for predicting Class with the model
expredROC <- prediction(ex.pred, caseDataClean$Attrition)
experf <- performance(expredROC, measure = "tpr", x.measure = "fpr")

aucEx <- performance(expredROC, measure = "auc")
aucEx <- aucEx@y.values[[1]]
  
plot(experf)
plot(experf, colorize=TRUE)
plot(experf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7), ylab="Sensitivity (TP Rate)",xlab="Specificty (TN Rate)", main = "External CV -  ROC Curve")  
  




###############################################################################################################################
###############################################################################################################################
#Naive Bayes Model ############################################################################################################
###############################################################################################################################
###############################################################################################################################



#Select Features############################################################
NBmodel = naiveBayes(Attrition~ OverTime + totalYears + raise + BusinessTravel + Education+ 
                       PercentSalaryHike+ID+Age+DistanceFromHome +JobSatisfaction +WorkLifeBalance +
                       MaritalStatus + EnvironmentSatisfaction +RelationshipSatisfaction + satisfaction+
                       StockOptionLevel+PerformanceRating+MonthlyIncome, data = caseDataClean, laplace=1)

#Check Inital Performance
nb.pred = predict(NBmodel, newdata=caseDataClean[,-1],type="raw")
nb.pred = as.data.frame(nb.pred)
nb.pred$Attrition = if_else(nb.pred$Yes> 0.4,"Yes","No")
confusionMatrix(table(nb.pred$Attrition, caseDataClean$Attrition))

caseDataClean %>%keep(is.factor) %>% names

#Select Features
caseDataCleanNB = caseDataClean %>%
  dplyr::select(Attrition,OverTime , totalYears , raise , BusinessTravel , Education, 
           PercentSalaryHike,ID,Age,DistanceFromHome ,JobSatisfaction ,WorkLifeBalance ,
           MaritalStatus , EnvironmentSatisfaction ,RelationshipSatisfaction , satisfaction,
           StockOptionLevel,PerformanceRating, MonthlyIncome)


#Create Training and Testing Sets ##########################################  
set.seed(100)
trainIndices = sample(seq(1:length(caseDataCleanNB$Attrition)),round(.75*length(caseDataCleanNB$Attrition)))
NBtrain = caseDataCleanNB[trainIndices,]
NBtest = caseDataCleanNB[-trainIndices,]

#Internal Cross Validation ################################################
x = NBtrain[,-1]
y = NBtrain[,1]
yTest = NBtest[,1]
tcontrol = trainControl(method='cv',number=25
                        , classProbs = TRUE
                        , summaryFunction = twoClassSummary)
model = train(x,y,
              method = 'nb',
              trControl=tcontrol
              )

#Model Performance
#AUC Curve----
nb.pred = predict(NBmodel, newdata=NBtest[,-1],type="raw")
nb.pred = as.data.frame(nb.pred)
nb.pred$Attrition = if_else(nb.pred$Yes> 0.45,"Yes","No")
confusionMatrix(table(nb.pred$Attrition, NBtest$Attrition))

nb.pred.ex = predict(NBmodel, newdata=caseDataCleanNB[,-1],type="raw")
nb.pred.ex = as.data.frame(nb.pred.ex)
nb.pred.ex$Attrition = if_else(nb.pred.ex$Yes> 0.45,"Yes","No")
confusionMatrix(table(nb.pred.ex$Attrition, caseDataCleanNB$Attrition))

#AUC Curve----
predNB = predict(NBmodel, newdata = NBtest[,-1], type="raw")
predNB = as.data.frame(predNB)
predNB = na.omit(predNB)
predNB$Attrition = if_else(predNB$Attrition> 0.4,"Attrition","No.Attrition")

predNB <- prediction(predNB, NBtest$Attrition)
perf <- performance(predNB, measure = "tpr", x.measure = "fpr")

aucInternalNB <- performance(predNB, measure = "auc")
aucInternalNB <- aucInternalNB@y.values[[1]]
#Parameter tunning
plot(perf)
plot(perf, colorize=TRUE)
plot(perf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7), ylab="True Positive Rate",xlab="True Negative Rate")


#Validation----
AccHolder = numeric(100)
SensHolder = numeric(100)
SpecHolder = numeric(100)

nb.pred = predict(NBmodel, newdata=NBtest[,-1],type="raw")
nb.pred = as.data.frame(nb.pred)
nb.pred$Attrition = if_else(nb.pred$Yes> 0.4,"Yes","No")
confusionMatrix(table(nb.pred$Attrition, NBtest$Attrition))


nb.pred = predict(NBmodel, newdata=caseDataCleanNB[,-1],type="raw")
nb.pred = as.data.frame(nb.pred)
nb.pred$Attrition = if_else(nb.pred$Yes> 0.4,"Yes","No")
confusionMatrix(table(nb.pred$Attrition, caseDataCleanNB$Attrition))

model = naiveBayes(NBtraining[,-1],NBtraining$Attrition, laplace=1 )
nb.predict = predict(model, newdata=NBtesting[,-1],type="raw")
nb.predict = as.data.frame(nb.pred)
nb.predict$Attrition = if_else(nb.pred$Yes> 0.4,"Yes","No")
CM = confusionMatrix(table(NBtesting$Attrition,nb.predict$Attrition))

for (seed in 1:100)
{
  set.seed(seed)
  trainIndex = sample(seq(1:length(caseDataCleanNB$Attrition)),round(.75*length(caseDataCleanNB$Attrition)))
  NBtraining = caseDataCleanNB[trainIndex,]
  NBtesting = caseDataCleanNB[-trainIndex,]
  
  model = naiveBayes(NBtraining[,-1],NBtraining$Attrition, laplace=1 )
  nb.predict = predict(model, newdata=NBtesting[,-1],type="raw")
  nb.predict = as.data.frame(nb.pred)
  nb.predict$Attrition = if_else(nb.pred$Yes> 0.4,"Yes","No")
  CM = confusionMatrix(table(NBtesting$Attrition,nb.predict$Attrition))
  

  
  AccHolder[seed] = CM$overall[1]
  SensHolder[seed] = CM$byClass[1]
  SpecHolder[seed] = CM$byClass[2]
}

mean(AccHolder)
mean(SensHolder)
mean(SpecHolder)


