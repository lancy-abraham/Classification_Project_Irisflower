#################################
###  CASE STUDY ANALSYSIS
#################################
##### Importing data
data1=read.csv(###your code here)


#### Data preprocessing according to your data and problem statement
######### Filtering relevant columns needed for analysis
########################################################

### Histogram of the response variable ###
qplot(###your code here)

### Obtaining descriptive statistics ###

install.packages("pastecs") # Install package 'pastecs' needed for obtaining descriptive stats 
library(pastecs)

stat.desc(###your code here) # stat_desc(): function for displaying the descriptive statistics - mean, median, SD etc.

###perfrom shapiro test
shapiro.test(###your code here)

###perform t test
t.test(###your code here)

### Creating training and test set
set.seed(123)
indx=sample(1:nn,0.9*nn)
traindata=data_an[indx,]
testdata=data_an[-indx,]

#### Fitting full logistic regression (LR) model with all features
fullmod=glm(###your code here)
summary(fullmod)


#### Selecting features for fitting reduced logistic regression model
library(MASS)
step=stepAIC(fullmod)

mod2=glm(###your code here)
summary(mod2)

### predicting success probabilities using the LR model
pred_prob=predict(###your code here)
hist(pred_prob)

### predicting success probability for an individual

#### Plotting ROC 
library(pROC)
roc1=roc(###your code here)
plot(roc1)
roc1$auc


#### Using ROC in deciding threshold
thres=data.frame(###your code here)
thres[###your code here]

library(caret)
pred_Y=ifelse(###your code here)
confusionMatrix(###your code here)



###############################
## Random Forest
###############################
library(randomForest)
 
###create train data

###RF model
modRF=randomForest(###your code here)
modRF

###create test data


predict(###your code here)
