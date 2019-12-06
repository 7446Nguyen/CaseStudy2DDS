options(scipen=999)
options(max.print=100000)

#load libraries####################################################################################
library(pacman)
p_load(tidyverse, skimr, caret,MASS)

#Model Selection###############################################################################################################
###############################################################################################################################
### Initial Models
model.forward.Start.lm <- lm(MonthlyIncome ~ 1, data = caseDataClean)
model.Allvar.lm <- lm(MonthlyIncome ~ ., data = caseDataClean)
########################################################################

#### Forward Selection
model.Forward.lm <- stepAIC(model.forward.Start.lm, direction = "forward", trace = F, scope = formula(model.Allvar.lm))

summary(model.Forward.lm)
model.Forward$anova
#################################### Forward Selection Model Suggestion
forward.lm <- lm(MonthlyIncome ~ JobLevel + raise + PercentSalaryHike + 
                   JobRole + totalYears + NumCompaniesWorked + BusinessTravel + 
                   DailyRate + Gender + YearsSinceLastPromotion
                 , data=caseDataClean)

summary(forward.lm)
########################################################################

# Backward Elimination
model.Backward.lm <- stepAIC(model.Allvar.lm, direction = "backward", trace = F, scope = formula(model.forward.Start.lm))
summary(model.Backward.lm)
model.Backward$anova
#################################### Backward Elimination Model Suggestion
back.lm <- lm(MonthlyIncome ~ BusinessTravel + DailyRate + Gender + 
                JobLevel + JobRole + NumCompaniesWorked + PercentSalaryHike + 
                YearsSinceLastPromotion + totalYears + raise
              , data=caseDataClean)

summary(back.lm)
back.glm$aic
########################################################################


# Stepwise Regression
model.Stepwise <- stepAIC(model.Allvar.lm, direction = "both", trace = F)
summary(model.Stepwise)
model.Stepwise$anova
#################################### Stepwise Regression Model Suggestion
step.lm <- lm(MonthlyIncome ~ BusinessTravel + DailyRate + Gender + 
                JobLevel + JobRole + NumCompaniesWorked + PercentSalaryHike + 
                YearsSinceLastPromotion + totalYears + raise
              , data=caseDataClean)

summary(step.lm)
step.lm$terms

vif(step.lm)

# Model Selection Statistics AIC and RMSE
lmmodel_stats <- data.frame(cbind(rbind("RMSE", "Multiple R-squared"),rbind( "723.9", "0.9758"), rbind("742.3", "0.9742")))
names(lmmodel_stats)[1]="Statistic"
names(lmmodel_stats)[2]="Initial Stepwise Model"
names(lmmodel_stats)[3]="Cross Validated Model"




#Cross Validation #############################################################################################################
###############################################################################################################################
#K-fold CV

set.seed(100)
Train <- createDataPartition(caseDataClean$MonthlyIncome, p=0.75, list=FALSE)
training <- caseDataClean[ Train, ]
testing <- caseDataClean[ -Train, ]

#train for specficity??? option
ctrl <- trainControl(method = "repeatedcv",
                     number = 25,
                     repeats = 10,
                     summaryFunction = defaultSummary,
                     allowParallel = T
                     
)

lm_model.cv <- train(MonthlyIncome ~ BusinessTravel + DailyRate + Gender + 
                       JobLevel + JobRole + NumCompaniesWorked + PercentSalaryHike + 
                       YearsSinceLastPromotion + totalYears + raise,
                     data=training, 
                     method="ridge", #or lasso
                     trControl = ctrl, 
                     tuneLength = 5,
                     #tuneGrid = lambdaGrid,
                     metric = "RMSE")


#generate LM predictions
#Internal CV

lm_predictions = predict(lm_model.cv,testing)
lm_predictions = predict(lm_model.cv,testing)
