1. 
#OBjective : To find out total Revenue in from of "Customer Lifetime Value" obtained
#            with the relationship with customers on various parameter.

2.
#Targetvariable - "Customer Lifetime Value(CLV)"

3.
#Loading the raw Data
InPutData=read.csv('C:/Users/harsha/Desktop/Stats+R/CSV Files/Fn-UseC_-Marketing-Customer-Value-Analysis.csv',
                   na.strings=c(""," ","NA","NULL"), stringsAsFactors = T)
InPutData

4.
#Exploring the Dataset - look for what kind of Variable in Data

head(InPutData,10)
str(InPutData)

5.
#Removing Useless column in the data , and explore the rest

Uselesscolumns=c("Customer","Effective.To.Date")

InPutData[ ,Uselesscolumns]=NULL

head(InPutData,6)
str(InPutData)

6.
# TargetVariable is in continuous nature - Regression Model

7.
#checking and treating Missing Values

#checking Missing Values

colSums(is.na(InPutData))

#No missing values in this data,hence processing further

############################################################################

8. 
#detection of outliers and treatment of extreme outliers 

#Customer.Lifetime.Value

boxplot(InPutData$Customer.Lifetime.Value, horizontal = T)

max(InPutData$Customer.Lifetime.Value)

quantiles=quantile(InPutData$Customer.Lifetime.Value, c(0.98, 0.99, 0.999, 0.9993))

quantiles

quantiles_final=quantile(InPutData$Customer.Lifetime.Value, 0.9993)

quantiles_final

InPutData$Customer.Lifetime.Value=ifelse(InPutData$Customer.Lifetime.Value>quantiles_final,quantiles_final, InPutData$Customer.Lifetime.Value)

boxplot(InPutData$Customer.Lifetime.Value, horizontal = T)
max(InPutData$Customer.Lifetime.Value)

# Total.Claim.Amount

boxplot(InPutData$Total.Claim.Amount, horizontal = T)

max(InPutData$Total.Claim.Amount)

quantiles=quantile(InPutData$Total.Claim.Amount, c(0.98, 0.99, 0.999, 0.9996))

quantiles

quantiles_final=quantile(InPutData$Total.Claim.Amount, 0.9996)

quantiles_final

InPutData$Total.Claim.Amount=ifelse(InPutData$Total.Claim.Amount>quantiles_final,quantiles_final, InPutData$Total.Claim.Amount)

boxplot(InPutData$Total.Claim.Amount, horizontal = T)

max(InPutData$Total.Claim.Amount)


##################################################################################################################
9.
#exploring each "Potential"predictor for Distribution and Quality

# Exploring Single Continuous feature

hist(InPutData$Customer.Lifetime.Value)
summary(InPutData$Customer.Lifetime.Value)

#Exploring Multiple Continuous features

ColsForHist=c("Customer.Lifetime.Value","Income","Monthly.Premium.Auto","Months.Since.Last.Claim",
              "Months.Since.Policy.Inception","Type.of.Open.Complaints","Type.of.Policies",
              "Total.Claim.Amount")

ColsForHist
par(mfrow=c(2,4))

for (hist_cols in ColsForHist) {
  hist(InPutData[,c(hist_cols)], main = paste('Histogram of:',hist_cols),
       col=brewer.pal(8,"Paired"))
}

library(RColorBrewer)

#Exploring Multiple categorical feature

ColsForBar=c("State","Response","Coverage","Education",
             "EmploymentStatus","Gender","Location.Code","Marital.Status","Policy.Type",
             "Policy","Renew.Offer.Type","Sales.Channel","Vehicle.Class","Vehicle.Size")

ColsForBar
par(mfrow=c(3,5))
for (bar_cols in ColsForBar){
  barplot(table(InPutData[,c(bar_cols)]), main=paste('Barplot of:',bar_cols), 
          col=brewer.pal(8,"Paired"))
}

############################################################################
10.
# Bivariate Analysis

#Visual Relationship between Predictors and Targetvariable 

#Continuous Vs Continuous - Scatter Plot

#Single continuous Variable

par(mfrow=c(1,1))

plot(x = InPutData$Monthly.Premium.Auto, y = InPutData$Customer.Lifetime.Value, col ="Blue")

#Multiple Continuous Variable

continuousCols=c("Customer.Lifetime.Value","Income","Monthly.Premium.Auto","Months.Since.Last.Claim",
                 "Months.Since.Policy.Inception","Type.of.Open.Complaints","Type.of.Policies",
                 "Total.Claim.Amount")

continuousCols

plot(InPutData[ ,continuousCols], col = "Blue")



#Continuous Vs Categorical Visual Analysi : BOXPLOT

Categorical_cols=c("State","Response","Coverage","Education",
                   "EmploymentStatus","Gender","Location.Code","Marital.Status","Policy.Type",
                   "Policy","Renew.Offer.Type","Sales.Channel","Vehicle.Class","Vehicle.Size")

par(mfrow=c(3,5))

for (bar_cols in Categorical_cols) {
  boxplot(Customer.Lifetime.Value~(InPutData[ ,c(bar_cols)]), data = InPutData,
          main =paste('Box Plot of:',bar_cols), col = brewer.pal(8,"Paired"))
}

##############################################################################
11.
# strength of relationship between predictors and target variables.

# Continuous Vs Continuous --Correlation test 

# Single continuous variable

cor(InPutData[ ,c('Customer.Lifetime.Value','Monthly.Premium.Auto')], use = "complete.obs")

# Positive relation between CLV and MPA as R is positive

# Multiple Continuous Variable

continuousCols=c("Customer.Lifetime.Value","Income","Monthly.Premium.Auto","Months.Since.Last.Claim",
                 "Months.Since.Policy.Inception","Type.of.Open.Complaints","Type.of.Policies",
                 "Total.Claim.Amount")
continuousCols

corrData=cor(InPutData[ ,continuousCols], use = "complete.obs")
corrData

#Final Continuous columns to be selected for modeling 
#The correlation is very low for almost all the columns, hence decreasing the threshold

names(corrData['Customer.Lifetime.Value',][abs(corrData['Customer.Lifetime.Value',])>0.2])

#"Monthly.Premium.Auto" and "Total.Claim.Amount" -good variable

#### Continuous Vs Categorical Correlation strength - ANOVA#########

#H0 := variables are not Correlated

colsForAnova=c("State","Response","Coverage","Education",
               "EmploymentStatus","Gender","Location.Code","Marital.Status","Policy.Type",
               "Policy","Renew.Offer.Type","Sales.Channel","Vehicle.Class","Vehicle.Size")

colsForAnova

for (Aovcols in colsForAnova) {
  Anovaresult=summary(aov(InPutData$Customer.Lifetime.Value~InPutData[ ,c(Aovcols)]))
  print(Aovcols)
  print(Anovaresult)
}


#shows Coverage,Education, Employment status, Marital.status, Renew.offer.Type, Vehicle.class are
# correlated with Taregtvariable CLV bases on ANOVA Result

###############################################################################################
12.
#Generating Data for Machine Learning

TargetVariableName=c('Customer.Lifetime.Value')

# Choosing multiple Predictors which may have relation with Target Variable
# Based on the exploratory data analysis

BestPredictorName=c("Monthly.Premium.Auto","Total.Claim.Amount","Coverage","Education",
                    "EmploymentStatus","Marital.Status","Renew.Offer.Type","Vehicle.Class")
BestPredictorName

## Extracting Target and predictor variables from data to create a generic dataset

TargetVariable=InPutData[ ,c(TargetVariableName)]
str(TargetVariable)

# Selecting all other columns as Predictors apart from target variable

PredictorVariable=InPutData[ ,BestPredictorName]
str(PredictorVariable)

##Creating the final data to be used for ML

DataForML=data.frame(TargetVariable,PredictorVariable)
str(DataForML)
head(DataForML)

###########################################################################################
13.

# Sampling | Splitting data into 70% for training 30% for testing

set.seed(123)

TrainingSampleIndex=sample(1:nrow(DataForML), size = 0.7*nrow(DataForML))
TrainingSampleIndex

DataForMLTrain=DataForML[TrainingSampleIndex, ]
DataForMLTest=DataForML[ -TrainingSampleIndex, ]
dim(DataForMLTrain)
dim(DataForMLTest)
head(DataForMLTest)
head(DataForMLTrain)
#####################################################################################
14.
# Creating Predictive models on training data to check the accuracy of each algorithm

#Linear Regression 
startTime=Sys.time()

Model_Reg=lm(TargetVariable~., data = DataForMLTrain)
summary(Model_Reg)

endTime=Sys.time()
endTime-startTime

#EmploymentStatus ==Unemployed().9243)-removed
Model_Reg_1=lm(TargetVariable~ Monthly.Premium.Auto+Total.Claim.Amount+Coverage
               +Education+I(EmploymentStatus=="Employed")+I(EmploymentStatus=="Retired")+
                 I(EmploymentStatus=="Medical Leave")
               +Marital.Status+Renew.Offer.Type+Vehicle.Class
                 , data = DataForMLTrain)

summary(Model_Reg_1)


#Marital.Status==Married(0.87576)-removed
Model_Reg_2=lm(TargetVariable~ Monthly.Premium.Auto+Total.Claim.Amount+Coverage
               +Education+I(EmploymentStatus=="Employed")+I(EmploymentStatus=="Retired")+
                 I(EmploymentStatus=="Medical Leave")
               +I(Marital.Status=="Single")+Renew.Offer.Type+Vehicle.Class
               , data = DataForMLTrain)
summary(Model_Reg_2)

#EmploymentStatus==Retired(0.84)-removed
Model_Reg_3=lm(TargetVariable~ Monthly.Premium.Auto+Total.Claim.Amount+Coverage
               +Education+I(EmploymentStatus=="Employed")+
                 I(EmploymentStatus=="Medical Leave")
               +I(Marital.Status=="Single")+Renew.Offer.Type+Vehicle.Class
               , data = DataForMLTrain)

summary(Model_Reg_3)

#Education==Doctor(0.7662)-removed
Model_Reg_4=lm(TargetVariable~ Monthly.Premium.Auto+Total.Claim.Amount+Coverage
               +I(Education=="College")+I(Education=="High School or Below")+I(Education=="Master")+I(EmploymentStatus=="Employed")+
                 I(EmploymentStatus=="Medical Leave")
               +I(Marital.Status=="Single")+Renew.Offer.Type+Vehicle.Class
               , data = DataForMLTrain)
summary(Model_Reg_4)

#EmploymentStatus==Medical Leave(0.68917)-removed
Model_Reg_5=lm(TargetVariable~ Monthly.Premium.Auto+Total.Claim.Amount+Coverage
               +I(Education=="College")+I(Education=="High School or Below")+I(Education=="Master")+I(EmploymentStatus=="Employed")
                 +I(Marital.Status=="Single")+Renew.Offer.Type+Vehicle.Class
               , data = DataForMLTrain)

summary(Model_Reg_5)

#Vehicle.Class==Two-Door Car(0.5695)-removed
Model_Reg_6=lm(TargetVariable~ Monthly.Premium.Auto+Total.Claim.Amount+Coverage
               +I(Education=="College")+I(Education=="High School or Below")+I(Education=="Master")+I(EmploymentStatus=="Employed")
               +I(Marital.Status=="Single")+Renew.Offer.Type+I(Vehicle.Class=="Luxury SUV")+I(Vehicle.Class=="Sports Car")+
                 I(Vehicle.Class=="SUV")+I(Vehicle.Class=="Luxury Car")
                ,data = DataForMLTrain)
summary(Model_Reg_6)

#Vehicle.Class ==Luxury Car(0.524)-removed
Model_Reg_7=lm(TargetVariable~ Monthly.Premium.Auto+Total.Claim.Amount+Coverage
               +I(Education=="College")+I(Education=="High School or Below")+I(Education=="Master")+I(EmploymentStatus=="Employed")
               +I(Marital.Status=="Single")+Renew.Offer.Type+I(Vehicle.Class=="Luxury SUV")+I(Vehicle.Class=="Sports Car")+
                 I(Vehicle.Class=="SUV")
               ,data = DataForMLTrain)

summary(Model_Reg_7)

#Coverage==Extended(0.6969)-removed
Model_Reg_8=lm(TargetVariable~ Monthly.Premium.Auto+Total.Claim.Amount+I(Coverage=="Premium")
               +I(Education=="College")+I(Education=="High School or Below")+I(Education=="Master")+I(EmploymentStatus=="Employed")
               +I(Marital.Status=="Single")+Renew.Offer.Type+I(Vehicle.Class=="Luxury SUV")+I(Vehicle.Class=="Sports Car")+
                 I(Vehicle.Class=="SUV")
               ,data = DataForMLTrain)

summary(Model_Reg_8)

##Coverage==Premium(0.5454)-removed
Model_Reg_9=lm(TargetVariable~ Monthly.Premium.Auto+Total.Claim.Amount
                +I(Education=="College")+I(Education=="High School or Below")+I(Education=="Master")+I(EmploymentStatus=="Employed")
                +I(Marital.Status=="Single")+Renew.Offer.Type+I(Vehicle.Class=="Luxury SUV")+I(Vehicle.Class=="Sports Car")+
                  I(Vehicle.Class=="SUV")
                ,data = DataForMLTrain)


summary(Model_Reg_9)

#Vehicle.Class ==Sports Car(0.5278)-removed
Model_Reg_10=lm(TargetVariable~ Monthly.Premium.Auto+Total.Claim.Amount
                +I(Education=="College")+I(Education=="High School or Below")+I(Education=="Master")+I(EmploymentStatus=="Employed")
                +I(Marital.Status=="Single")+Renew.Offer.Type+I(Vehicle.Class=="Luxury SUV")+
                  I(Vehicle.Class=="SUV")
                ,data = DataForMLTrain)

summary(Model_Reg_10)

#Education == Master(0.4797)-removed
Model_Reg_11=lm(TargetVariable~ Monthly.Premium.Auto+Total.Claim.Amount
                +I(Education=="College")+I(Education=="High School or Below")+I(EmploymentStatus=="Employed")
                +I(Marital.Status=="Single")+Renew.Offer.Type+I(Vehicle.Class=="Luxury SUV")+
                  I(Vehicle.Class=="SUV")
                ,data = DataForMLTrain)

summary(Model_Reg_11)

#Education ==College(0.57019)-removed
Model_Reg_12=lm(TargetVariable~ Monthly.Premium.Auto+Total.Claim.Amount
                +I(Education=="High School or Below")+I(EmploymentStatus=="Employed")
                +I(Marital.Status=="Single")+Renew.Offer.Type+I(Vehicle.Class=="Luxury SUV")+
                  I(Vehicle.Class=="SUV")
                ,data = DataForMLTrain)
summary(Model_Reg_12)

#Renew.Offer.Type==Offer3(0.2366)-removed
Model_Reg_13=lm(TargetVariable~ Monthly.Premium.Auto+Total.Claim.Amount
                +I(Education=="High School or Below")+I(EmploymentStatus=="Employed")
                +I(Marital.Status=="Single")+I(Renew.Offer.Type=="Offer2")+I(Renew.Offer.Type=="Offer4")+I(Vehicle.Class=="Luxury SUV")+
                  I(Vehicle.Class=="SUV")
                ,data = DataForMLTrain)
summary(Model_Reg_13)

#Total.Claim.Amount(0.148)-removed
Model_Reg_14=lm(TargetVariable~ Monthly.Premium.Auto
                +I(Education=="High School or Below")+I(EmploymentStatus=="Employed")
                +I(Marital.Status=="Single")+I(Renew.Offer.Type=="Offer2")+I(Renew.Offer.Type=="Offer4")+I(Vehicle.Class=="Luxury SUV")+
                  I(Vehicle.Class=="SUV")
                ,data = DataForMLTrain)
summary(Model_Reg_14)

#Vehicle.Class ==Luxury SUV(0.098)-removed
Model_Reg_15=lm(TargetVariable~ Monthly.Premium.Auto
                +I(Education=="High School or Below")+I(EmploymentStatus=="Employed")
                +I(Marital.Status=="Single")+I(Renew.Offer.Type=="Offer2")+I(Renew.Offer.Type=="Offer4")+
                  I(Vehicle.Class=="SUV")
                ,data = DataForMLTrain)
summary(Model_Reg_15)

#Education ==High School or Below(0.055)-removed
Model_Reg_16=lm(TargetVariable~ Monthly.Premium.Auto+
                I(EmploymentStatus=="Employed")
                +I(Marital.Status=="Single")+I(Renew.Offer.Type=="Offer2")+I(Renew.Offer.Type=="Offer4")+
                  I(Vehicle.Class=="SUV")
                ,data = DataForMLTrain)
summary(Model_Reg_16)

#Multiple R-squared :0.1612 and Adjusted R-squared - 0.1605
###################################################################################
15.
##Test for Homoskedacity /Hetroskedacity

#H0: There is exists of Homoskedacity 

install.packages("lmtest")
library(lmtest)

bptest(Model_Reg_16)

# Pvalue <0.05 hence, we reject Null Hypothesis and say there is a exists of Heteroskedacity in data
###########################################################################################
16.
##Test for Serial Correlation (Auto-correlation)

# Ho:No autocorrelation

library(lmtest)
dwtest(Model_Reg_16)

#P value >0.05 , we accept the Null Hypotheis and say there is No Correlation 
##########################################################################################
17.
#Test for normality 

#H0: error is normally distributed

install.packages("nortest")
library(nortest)

resid=Model_Reg_16$residuals
ad.test(resid)

#P-value<0.05 hence we Reject Null Hypotheis and say Errors are normally distributed in Data
############################################################################################
18.
### Test For Multicolinearity

library(car)

VIF=vif(Model_Reg_16)
data.frame(VIF)

#We can see all value are very close to 1 and which is less than 5 
#hence we will keep all variables and check the accuracy on test data
###########################################################################################
19.
#checking accuracy on Testing Data

head(DataForMLTest)
DataForMLTest$Pred_LM=predict(Model_Reg_16, DataForMLTest)
head(DataForMLTest)

#####################################################################
20.
#Calculating  the absolute Percentage Error for each prediction

DataForMLTest$LM_APE=100*(abs(DataForMLTest$TargetVariable-DataForMLTest$Pred_LM)/
                            DataForMLTest$TargetVariable)
head(DataForMLTest)

MeanAPE=mean(DataForMLTest$LM_APE)
MedianAPE=median(DataForMLTest$LM_APE)
print(paste('## Mean Accuracy of Linear Regression Model is:', 100 - MeanAPE))
print(paste('## Median Accuracy of Linear Regression Model is:', 100 - MedianAPE))

############################################################################
1.
#Decision TREE######

library(party)

Model_CTREE=ctree(TargetVariable~., data = DataForMLTrain)
Model_CTREE

plot(Model_CTREE)

###########################################################################
2.
#Checking Accuracy of model on  Testing Data 

DataForMLTest$Pred_CTREE=as.numeric(predict(Model_CTREE, DataForMLTest))
head(DataForMLTest)

DataForMLTest$CTREE_APE=100*(abs(DataForMLTest$TargetVariable-DataForMLTest$Pred_CTREE)/
                               DataForMLTest$TargetVariable)
head(DataForMLTest)

print(paste('## Mean Accuracy of Decision tree Model is:', 100 -mean(DataForMLTest$CTREE_APE)))
print(paste('## Median Accuracy of Decision Tree Model is:', 100 - median(DataForMLTest$CTREE_APE)))


# Linear Regression provide better accuracy for Median hence we can consider Linear model
#over Decision Tree