#Data Cleaning and Wrangling

#load libraries####################################################################################
library(pacman)
p_load(tidyverse, dummies, skimr, readxl, janitor, GGally,corrplot)

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
caseData = caseData %>%
  dplyr::select(-c(StandardHours, EmployeeCount, Over18))

caseDataClean = caseData %>%
  mutate(Education = as.factor(Education)) %>%
  mutate(EnvironmentSatisfaction = as.factor(EnvironmentSatisfaction)) %>%
  mutate(JobInvolvement = as.factor(JobInvolvement)) %>%
  mutate(JobLevel = as.factor(JobLevel)) %>%
  mutate(JobSatisfaction = as.factor(JobSatisfaction)) %>%
  mutate(PerformanceRating = as.factor(PerformanceRating)) %>%
  mutate(RelationshipSatisfaction = as.factor(RelationshipSatisfaction)) %>%
  mutate(StockOptionLevel = as.factor(StockOptionLevel)) %>%
  mutate(TrainingTimesLastYear = as.factor(TrainingTimesLastYear)) %>%
  mutate(WorkLifeBalance = as.factor(TrainingTimesLastYear))%>%
  mutate(totalYears = (YearsWithCurrManager + YearsInCurrentRole + YearsAtCompany + TotalWorkingYears)/4) %>%
  mutate(BusinessTravel = if_else(BusinessTravel == "Non-Travel","Non-Travel","Travel")) %>%
  mutate(Attrition = as.factor(Attrition)) %>%
  mutate(raise = PercentSalaryHike/100 * MonthlyIncome) %>%
  mutate(satisfaction = (as.numeric(EnvironmentSatisfaction) + as.numeric(RelationshipSatisfaction))/2) %>%
  mutate(HourlyRate = as.double(HourlyRate))%>%
  mutate(overtimeRate = if_else(OverTime =="Yes", HourlyRate*1.5, HourlyRate))%>%
  mutate_if(is.character, as.factor)

caseDataClean = as.data.frame(caseDataClean)
caseDataClean = caseDataClean %>%
  dplyr::select(-c(YearsWithCurrManager,YearsInCurrentRole,YearsAtCompany,TotalWorkingYears))

caseDataCleanLogM = caseDataClean %>%
  mutate(Attrition = if_else(Attrition == "No",0,1)) 


#noAttrition cat conversion

noAttritionClean = noAttrition %>%
  mutate(Education = as.factor(Education)) %>%
  mutate(EnvironmentSatisfaction = as.factor(EnvironmentSatisfaction)) %>%
  mutate(JobInvolvement = as.factor(JobInvolvement)) %>%
  mutate(JobLevel = as.factor(JobLevel)) %>%
  mutate(JobSatisfaction = as.factor(JobSatisfaction)) %>%
  mutate(PerformanceRating = as.factor(PerformanceRating)) %>%
  mutate(RelationshipSatisfaction = as.factor(RelationshipSatisfaction)) %>%
  mutate(StockOptionLevel = as.factor(StockOptionLevel)) %>%
  mutate(TrainingTimesLastYear = as.factor(TrainingTimesLastYear)) %>%
  mutate(WorkLifeBalance = as.factor(TrainingTimesLastYear))%>%
  mutate(totalYears = (YearsWithCurrManager + YearsInCurrentRole + YearsAtCompany + TotalWorkingYears)/4) %>%
  mutate(BusinessTravel = if_else(BusinessTravel == "Non-Travel","Non-Travel","Travel")) %>%
  mutate(raise = PercentSalaryHike/100 * MonthlyIncome) %>%
  mutate(satisfaction = (as.numeric(EnvironmentSatisfaction) + as.numeric(RelationshipSatisfaction))/2) %>%
  mutate(HourlyRate = as.double(HourlyRate))%>%
  mutate(overtimeRate = if_else(OverTime =="Yes", HourlyRate*1.5, HourlyRate))%>%
  mutate_if(is.character, as.factor)

noAttritionClean = as.data.frame(noAttritionClean)
noAttritionClean = noAttritionClean %>%
  dplyr::select(-c(YearsWithCurrManager,YearsInCurrentRole,YearsAtCompany,TotalWorkingYears,StandardHours, EmployeeCount, Over18))


#noSalary cat conversion
noSalaryClean = noSalary %>%
  mutate(Education = as.factor(Education)) %>%
  mutate(EnvironmentSatisfaction = as.factor(EnvironmentSatisfaction)) %>%
  mutate(JobInvolvement = as.factor(JobInvolvement)) %>%
  mutate(JobLevel = as.factor(JobLevel)) %>%
  mutate(JobSatisfaction = as.factor(JobSatisfaction)) %>%
  mutate(PerformanceRating = as.factor(PerformanceRating)) %>%
  mutate(RelationshipSatisfaction = as.factor(RelationshipSatisfaction)) %>%
  mutate(StockOptionLevel = as.factor(StockOptionLevel)) %>%
  mutate(TrainingTimesLastYear = as.factor(TrainingTimesLastYear)) %>%
  mutate(WorkLifeBalance = as.factor(TrainingTimesLastYear))%>%
  mutate(totalYears = (YearsWithCurrManager + YearsInCurrentRole + YearsAtCompany + TotalWorkingYears)/4) %>%
  mutate(BusinessTravel = if_else(BusinessTravel == "Non-Travel","Non-Travel","Travel")) %>%
  mutate(Attrition = as.factor(Attrition)) %>%
  mutate(raise = PercentSalaryHike/100 * MonthlyRate) %>%
  mutate(satisfaction = (as.numeric(EnvironmentSatisfaction) + as.numeric(RelationshipSatisfaction))/2) %>%
  mutate(HourlyRate = as.double(HourlyRate))%>%
  mutate(overtimeRate = if_else(OverTime =="Yes", HourlyRate*1.5, HourlyRate))%>%
  mutate_if(is.character, as.factor)

noSalaryClean = as.data.frame(noSalaryClean)
noSalaryClean = noSalaryClean %>%
  dplyr::select(-c(YearsWithCurrManager,YearsInCurrentRole,YearsAtCompany,TotalWorkingYears,StandardHours, EmployeeCount, Over18))

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
  mutate(StockOptionLevel = as.factor(StockOptionLevel)) %>%
  mutate(TrainingTimesLastYear = as.factor(TrainingTimesLastYear)) %>%
  mutate(WorkLifeBalance = as.factor(TrainingTimesLastYear))%>%
  mutate(totalYears = (YearsWithCurrManager + YearsInCurrentRole + YearsAtCompany + TotalWorkingYears)/4) %>%
  mutate(BusinessTravel = if_else(BusinessTravel == "Non-Travel","Non-Travel","Travel")) %>%
  mutate(Attrition = as.factor(Attrition)) %>%
  mutate(raise = PercentSalaryHike/100 * MonthlyRate) %>%
  mutate(satisfaction = (as.numeric(EnvironmentSatisfaction) + as.numeric(RelationshipSatisfaction))/2) %>%
  mutate(HourlyRate = as.double(HourlyRate))%>%
  mutate(overtimeRate = if_else(OverTime =="Yes", HourlyRate*1.5, HourlyRate))%>%
  mutate_if(is.character, as.factor)

noSalaryClean = as.data.frame(noSalaryClean)
noSalaryClean = noSalaryClean %>%
  dplyr::select(-c(YearsWithCurrManager,YearsInCurrentRole,YearsAtCompany,TotalWorkingYears,StandardHours, EmployeeCount, Over18))
skim(noSalary)

#EDA and Correlation plots #####################################################################
caseData_numeric <- caseData %>% keep(is.numeric)
#Correlation plot for Quantitative Variables
corrplot::corrplot(cor(caseData_numeric)
                   , title = "Correlation of Quantitative Predictor Variables"
                   , type = "lower"
                   , tl.pos = "ld"
                   , method = "square"
                   , tl.cex = 0.65
                   , tl.col = 'red'
                   , order = "alphabet"
                   , diag = F
                   , mar=c(0,0,5,0)
                   , bg="ivory1"
                   ,tl.srt=.05
)

#'TotalWorkingYears and Monthly income r = 0.78
#'TotalWorkingYears and Age r = 0.65
#'YearsInCurrentRole and YearsAtCompany r = 0.78
#'YearsSincelastPromotion and YearsAtCompany r = 0.64
#'YearsWithCurrManger and YearsAtCompany r = 0.77
#'YearsWithCurrManger and YearsInCurrentRole r = 0.71

caseDataClean_numeric <- caseDataClean %>% keep(is.numeric)
#Correlation plot for Quantitative Variables
corrplot::corrplot(cor(caseDataClean_numeric)
                   , title = "Correlation - Post Feature Engineering"
                   , type = "lower"
                   , tl.pos = "ld"
                   , method = "square"
                   , tl.cex = 0.65
                   , tl.col = 'red'
                   , order = "alphabet"
                   , diag = F
                   , mar=c(0,0,5,0)
                   , bg="ivory1"
                   ,tl.srt=.05
)


caseDataCleanLogM = caseDataClean %>%
  dplyr::select(-c(raise))

caseDataClean_numericFinal <- caseDataCleanLogM %>% keep(is.numeric)
#Correlation plot for Quantitative Variables
corrplot::corrplot(cor(caseDataClean_numericFinal)
                   , title = "Final Correlation of Quantitative Predictor Variables"
                   , type = "lower"
                   , tl.pos = "ld"
                   , method = "square"
                   , tl.cex = 0.65
                   , tl.col = 'red'
                   , order = "alphabet"
                   , diag = F
                   , mar=c(0,0,5,0)
                   , bg="ivory1"
                   ,tl.srt=.05
)
caseDataClean_numeric %>%
  ggpairs()

#Distirbution plots
a=ggplot(caseDataClean_numeric, aes(x = ID, fill = ID)) + geom_histogram()
b=ggplot(caseDataClean_numeric, aes(x = Age, fill = Age)) + geom_histogram()
c=ggplot(caseDataClean_numeric, aes(x = DailyRate, fill = DailyRate)) + geom_histogram()
d=ggplot(caseDataClean_numeric, aes(x = DistanceFromHome, fill = DistanceFromHome)) + geom_histogram()
e=ggplot(caseDataClean_numeric, aes(x = EmployeeNumber, fill = EmployeeNumber)) + geom_histogram()

f=ggplot(caseDataClean_numeric, aes(x = HourlyRate, fill = HourlyRate)) + geom_histogram()
g=ggplot(caseDataClean_numeric, aes(x = MonthlyIncome, fill = MonthlyIncome)) + geom_histogram()
h=ggplot(caseDataClean_numeric, aes(x = MonthlyRate, fill = MonthlyRate)) + geom_histogram()
i=ggplot(caseDataClean_numeric, aes(x = NumCompaniesWorked, fill = NumCompaniesWorked)) + geom_histogram()
j=ggplot(caseDataClean_numeric, aes(x = PercentSalaryHike, fill = PercentSalaryHike)) + geom_histogram()

k=ggplot(caseDataClean_numeric, aes(x = YearsSinceLastPromotion, fill = YearsSinceLastPromotion)) + geom_histogram()
l=ggplot(caseDataClean_numeric, aes(x = totalYears, fill = totalYears)) + geom_histogram()
m=ggplot(caseDataClean_numeric, aes(x = raise, fill = raise)) + geom_histogram()
n=ggplot(caseDataClean_numeric, aes(x = satisfaction, fill = satisfaction)) + geom_histogram()
o=ggplot(caseDataClean_numeric, aes(x = overtimeRate, fill = overtimeRate)) + geom_histogram()

ggarrange(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,ncol=3,nrow=5)


#Job Satisfaction Categorical Analysis
ggplot(caseDataClean, aes(x = JobSatisfaction, fill = JobRole)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)+ 
  ylim(0,80)+
  facet_wrap(~JobRole)+ 
  theme(legend.position = "none")



