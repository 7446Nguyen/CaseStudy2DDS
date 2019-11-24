#Data Cleaning and Wrangling

#load libraries####################################################################################
library(pacman)
p_load(tidyverse, dummies, skimr, readxl, janitor, GGally)

#load data#########################################################################################
caseData = read.csv("./data/CaseStudy2-data.csv", 
               header=T, sep=",", strip.white=T, stringsAsFactors = F, na.strings=c(""))

noAttrition = read.csv("./data/CaseStudy2CompSet No Attrition.csv", 
                       header=T, sep=",", strip.white=T, stringsAsFactors = F, na.strings=c(""))

noSalary = read_excel("./data/CaseStudy2CompSet No Salary.xlsx")


#Look for duplicates #############################################################################
#log transform DistanceFromHome, MonthlyIncome???
#caseData de-duping
skim(caseData) # no NAs
caseData%>%
  get_dupes(EmployeeNumber,ID) # no dups

#noAttrition de-duping
skim(noAttrition) # no NAs
noAttrition%>%
  get_dupes(EmployeeNumber,ID) # no dups

#noSalary de-duping
skim(noSalary) # no NAs
noSalary%>%
  get_dupes(EmployeeNumber,ID) # no dups
 

#Convert quant to categorical #####################################################################

#case data cat conversion; (experiment with converting features with many levels to quant and visa versa, ex. NumCompaniesWorked)
caseDataClean = caseData %>%
  mutate(Education = as.factor(Education)) %>%
  mutate(EnvironmentSatisfaction = as.factor(EnvironmentSatisfaction)) %>%
  mutate(JobInvolvement = as.factor(JobInvolvement)) %>%
  mutate(JobLevel = as.factor(JobLevel)) %>%
  mutate(JobSatisfaction = as.factor(JobSatisfaction)) %>%
  mutate(PerformanceRating = as.factor(PerformanceRating)) %>%
  mutate(RelationshipSatisfaction = as.factor(RelationshipSatisfaction)) %>%
  mutate(StockOptionLevel = as.factor(RelationshipSatisfaction)) %>%
  mutate(TrainingTimesLastYear = as.factor(TrainingTimesLastYear)) %>%
  mutate(WorkLifeBalance = as.factor(TrainingTimesLastYear))

#check to see features have been converted to categorical
skim(caseDataClean)


#noAttrition cat conversion
noAttritionClean = noAttrition %>%
  mutate(Education = as.factor(Education)) %>%
  mutate(EnvironmentSatisfaction = as.factor(EnvironmentSatisfaction)) %>%
  mutate(JobInvolvement = as.factor(JobInvolvement)) %>%
  mutate(JobLevel = as.factor(JobLevel)) %>%
  mutate(JobSatisfaction = as.factor(JobSatisfaction)) %>%
  mutate(PerformanceRating = as.factor(PerformanceRating)) %>%
  mutate(RelationshipSatisfaction = as.factor(RelationshipSatisfaction)) %>%
  mutate(StockOptionLevel = as.factor(RelationshipSatisfaction)) %>%
  mutate(TrainingTimesLastYear = as.factor(TrainingTimesLastYear)) %>%
  mutate(WorkLifeBalance = as.factor(TrainingTimesLastYear))

skim(noAttritionClean)


#noSalary cat conversion
noSalaryClean = noSalary %>%
  mutate(Education = as.factor(Education)) %>%
  mutate(EnvironmentSatisfaction = as.factor(EnvironmentSatisfaction)) %>%
  mutate(JobInvolvement = as.factor(JobInvolvement)) %>%
  mutate(JobLevel = as.factor(JobLevel)) %>%
  mutate(JobSatisfaction = as.factor(JobSatisfaction)) %>%
  mutate(PerformanceRating = as.factor(PerformanceRating)) %>%
  mutate(RelationshipSatisfaction = as.factor(RelationshipSatisfaction)) %>%
  mutate(StockOptionLevel = as.factor(RelationshipSatisfaction)) %>%
  mutate(TrainingTimesLastYear = as.factor(TrainingTimesLastYear)) %>%
  mutate(WorkLifeBalance = as.factor(TrainingTimesLastYear))

skim(noSalary)


#EDA and Correlation plots #####################################################################

caseDataClean_numeric <- caseDataClean %>% keep(is.numeric)

caseDataClean_numeric = caseDataClean_numeric %>%
  select(-StandardHours, -EmployeeCount)

corrplot::corrplot(cor(caseDataClean_numeric)
                   , title = "Correlation of Predictor Variables, Before Variable Elimination"
                   , order = "alphabet"
                   , tl.cex = 0.7
                   , method = "number"
                   , diag = T
                   , type = "lower"

)

#'TotalWorkingYears and Monthly income r = 0.78
#'TotalWorkingYears and Age r = 0.65
#'YearsInCurrentRole and YearsAtCompany r = 0.78
#'YearsSincelastPromotion and YearsAtCompany r = 0.64
#'YearsWithCurrManger and YearsAtCompany r = 0.77
#'YearsWithCurrManger and YearsInCurrentRole r = 0.71

caseDataClean_numeric %>%
  ggpairs()
