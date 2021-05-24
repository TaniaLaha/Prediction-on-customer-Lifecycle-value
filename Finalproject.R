InsuranceData = read.csv('C:/Users/DELL/Desktop/Ivy_assignments/Stats+R/final_project/Final R Project IVY/Fn-UseC_-Marketing-Customer-Value-Analysis.csv',na.strings = c(""," ","NA","NULL"),stringsAsFactors = TRUE)
InData = Insurancedata
str(InData)
InData[,c('Customer','Effective.To.Date')]<- NULL
View(is.na(InData))
colSums(is.na(InData))

Functionmode = function(inpdata){
  Modevalue = names(table(inpdata)[table(inpdata)==max(table(inpdata))])
  return(inpdata)
}

names(InData)
cont_col=c("Customer.Lifetime.Value","Income","Monthly.Premium.Auto","Months.Since.Last.Claim","Months.Since.Policy.Inception","Type.of.Open.Complaints","Type.of.Policies","Total.Claim.Amount") 
cat_col=c("State","Response","Coverage","Education","EmploymentStatus","Gender","Location.Code","Marital.Status","Policy.Type","Policy","Renew.Offer.Type","Sales.Channel","Vehicle.Class","Vehicle.Size")

sapply(InData[,cont_col],mean)
sapply(InData[,cont_col],median)
sapply(InData[,cat_col],Functionmode)

library(RColorBrewer)

par(mfrow=c(2,4))
for(i in cont_col){
  hist(InData[,c(i)],main = paste("Chart of: ",i),col=brewer.pal(12,"Paired"))
}

par(mfrow=c(2,4))
for(i in cont_col){
  boxplot(InData[,c(i)],main = paste("Chart of: ",i),col=brewer.pal(12,"Paired"))
}

par(mfrow=c(3,5))
for(i in cat_col){
  plot(InData[,c(i)],main = paste("Chart of: ",i),col=brewer.pal(12,"Paired"))
}

##### Bi-Virate Analysis ######
# Continuous Vs Continuous --- Scatter plot
par(mfrow=c(1,1))
plot(InData[,cont_col],col='green')
View(InData)

# Continuous Vs Categorical Visual analysis: Boxplot
par(mfrow=c(1,3))
for(t in cat_col){
  boxplot(Customer.Lifetime.Value ~ InData[,t], data = InData , main = paste("Boxplot of: ",t),col=brewer.pal(8,"Paired"))
}

#### Strength of Relationship between predictor and target variable ####
###### Correlation Test ########
#### Continuous vs Continuous #######
Corrdata = cor(InData[,cont_col], use = "complete.obs")
Corrdata

#### Monthly premium auto and Total claim amount are strongly correlated with CLV ######

Corrdata['Customer.Lifetime.Value',]
abs(Corrdata['Customer.Lifetime.Value',])>0.2
names(Corrdata['Customer.Lifetime.Value',][abs(Corrdata['Customer.Lifetime.Value',])>0.2])

#### Except CLV rest 2 columns are potential predictor variables #######

#### Continuous vs Categorical ####
### Annova test###
for(i in cat_col){
  print(i)
  b = summary(aov(Customer.Lifetime.Value ~ InData[,i],data = InData))
  print(b)
}

### It is visible that Coverage , Employment status , Renew offer type and Vehicle class are strongly correlated with CLV ####

#### Education and marital status are lightly correlated ####
names(InData)
##### Preparing data for MLR #####
Targetvar = "Customer.Lifetime.Value"
bestpredictor = c('Monthly.Premium.Auto','Total.Claim.Amount','Coverage','EmploymentStatus','Renew.Offer.Type','Vehicle.Class')
Tarvar = InData[,Targetvar]
Bestpred = InData[,bestpredictor]

Mldata = data.frame(Tarvar,Bestpred)
str(Mldata)

## Spliting 70% data for Train and 30% for Test #######
TrainsampleIndex = sample(1:nrow(Mldata),size = 0.7*nrow(Mldata))
Traindata = Mldata[TrainsampleIndex,]
Traindata
Testdata = Mldata[-TrainsampleIndex,]
Testdata

dim(Traindata)
dim(Testdata)

### MLR #######
Model_reg = lm(Tarvar~.,data = Traindata)
summary(Model_reg)

Model2=lm(Tarvar~ Monthly.Premium.Auto+I(Vehicle.ClassSports Car)+I(Vehicle.ClassSUV)+Type.of.Policies,data=Traindata)
summary(Model2)

Testdata$Pred_LM = predict(Model_reg,Testdata)
head(Testdata)

## Checking accuracy for model on testing data #######
Testdata$Pred_LM = predict(Model_reg,Testdata)
tail(Testdata)

Testdata$LM_APE = 100*(abs((Testdata$Tarvar-Testdata$Pred_LM)/Testdata$Tarvar))
head(Testdata)

MeanApe = mean(Testdata$LM_APE)
MedianApe = median(Testdata$LM_APE)
print(paste('### Mean Accuracy of Linear Regression Model is: ', 100 - MeanApe))
print(paste('### Median Accuracy of Linear Regression Model is: ', 100 - MedianApe))

#### Decision Tree ######
library(party)
Model_CTREE = ctree(Tarvar ~ . , data = Traindata)

#to see the if-else statements:
##you'll be able to see what kind of rules the DT has created for this data
Model_CTREE

# Checking Accuracy of model on Testing data
##generating the predictions using decision tree and arriving at the accuracy
Testdata$Pred_Ctree = as.numeric(predict(Model_CTREE,Testdata))
head(Testdata)

Testdata$Ctree_APE = 100 * (abs((Testdata$Tarvar-Testdata$Pred_Ctree)/Testdata$Tarvar))
head(Testdata)

##we can see for each prediction what is the error I am committing
##for average accuracy: we take the average of all the errors and subtract it from 100

print(paste('### Mean Accuracy of Decision tree Model is: ', 100 - mean(Testdata$Ctree_APE)))
print(paste('### Median Accuracy of Decision tree Model is: ', 100 - median(Testdata$Ctree_APE)))

