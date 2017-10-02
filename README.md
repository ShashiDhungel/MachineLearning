# MachineLearning
This repository consists of ML algorithms used in credit risk modeling. Try it!

# Random Forest in R
library(haven)
library(tidyverse)
set.seed(1234)

# Using haven package to load sas files.
data<-read_sas("Your file path")

#For splitting data in training and test sets
ind<-sample(2, nrow(data), replace = TRUE, prob=c(0.7,0.3))

# library(randomForest)
x.rf <- randomForest(Dependent_Variable ~ independent_1 + independent_2 + independent_3, 
                     data=data[ind == 1,], importance = TRUE, ntree = 500)
                     
# predict classes for the evaluation data set
x.rf.pred <- predict(x.rf, newdata=data[ind == 2,])

# score the evaluation data set (extract the probabilities)
x.rf.prob <- predict(x.rf, type="prob", newdata=data[ind == 2,])

# Confusion matrix to evaluate error rate
tab<-table(x.rf.pred, data[ind == 2,]$Dependent_Variable)
tab

# Error rate
sum(diag(tab))/sum(tab)

#Check which variables are the most important ones

options(repr.plot.width=8, repr.plot.height=5) # setting the height and width of the plot
varImpPlot(x.rf)

# Another way to look at cross tabulation using gmodels library
library(gmodels)
x.rf.crosstab<-data.frame(x.rf.pred, data[ind ==2,1])  
# here [ind ==2, 1] means we are cross tabulating agaisnt the first column where ind = 1 which is our test set.

colnames(x.rf.crosstab)<-c("Predicted", "Observed")
CrossTable(x=x.rf.crosstab$Predicted, y=x.rf.crosstab$Observed)
