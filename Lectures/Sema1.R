library(kernlab)
data(spam)
str(spam[,1:5])
?spam

#perform the subsampling

set.seed(3435)
trainIndicator = rbinom(4601,size=1,prob=0.5)
table(trainIndicator)

trainSpam = spam[trainIndicator==1,]
testSpam = spam[trainIndicator==0,]

#Exploratory data Analysis
names(trainSpam)
head(trainSpam)

table(trainSpam$type)

plot(log10(trainSpam$capitalAve + 1) ~ trainSpam$type) #since is a exploratory plot

plot(log10(trainSpam[,1:4]+1)) #pairwise relationships

#hierarchical clustering  
hcluster = hclust(dist(t(trainSpam[,1:57])))
plot(hcluster)

hClusterUpdated = hclust(dist(t(log10(trainSpam[,1:55]+1))))
plot(hClusterUpdated)

#Statistical prediction/modeling
##Logistic model
trainSpam$numtype = as.numeric(trainSpam$type)-1
costFunction = function(x,y) sum( x!=(y>0.5))
cvError = rep(NA,55)
library(boot)
for(i in 1:55){
  lmFormula = reformulate(names(trainSpam)[i], response = "numtype")
  glmFit = glm(lmFormula,family = "binomial",data = trainSpam )
  cvError[i] = cv.glm(trainSpam,glmFit, costFunction ,2)$delta[2]
}

## Which predictor has the minimum cross validated error
names(trainSpam)[which.min(cvError)]

## Use the best model from the group

predictionModel = glm(numtype ~ charDollar, family = "binomial" ,data = trainSpam)

predictionTest = predict(predictionModel,testSpam)

predictedSpam = rep("nonspam",dim(testSpam)[1])

##Classify as 'spam' for those with probability  >0.5
predictedSpam[predictionModel$fitted>0.5] = "spam"

table(predictedSpam,testSpam$type)

#Error rate

(61 + 458)/

