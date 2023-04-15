
## Call the relevant packages & if needed use "install.packages" to install the packages first.
library(readxl)
library(xlsx)
library(caret)## Please load package "caret" before "mlr"
library(mlr)
library(Amelia)
library(DataExplorer)
library(ggplot2)
library(grid)
library(gridExtra)
library(scales)
## Reading the input data
train = read_xlsx(choose.files())
dim(train)
missmap(train,col=c('orange','blue'),y.cex = 0.5,x.cex = 1.0)

str(train)
summary(train)
View(train)
dim(train)
attach(train)

##Convert the Remarks into Factor
train$Remarks<-as.factor(train$Remarks)

summary(train)
#Remove CM3 as it is not adding any value in the equation
train$CM3<-NULL
##Remove the Cell ID as well as it is not required at this moment
train$`Cell ID`<-NULL
#Convert AL1 and AL2 as factors
train$AL1<-as.factor(train$AL1)
train$AL2<-as.factor(train$AL2)
summary(train)
View(train)
table(Remarks)
par(mfrow=c(2,3))
b1<-boxplot(`No of days of KPI failure`,xlab="Days of KPI Failure",col="blue")
b2<-boxplot(KPI,xlab="KPI Value",col="blue")
b3<-boxplot(PM1,xlab="PM1 Value",col="blue")
b4<-boxplot(PM2,xlab="PM2 Value",col="blue")
b5<-boxplot(PM3,xlab="PM3 Value",col="blue")
b6<-boxplot(pm4,xlab="PM4 Value",col="blue")

##Handle Missing Values

imputed = impute(train, target = character(0), cols = list(AL1 = 0,AL2=0))

imputed = impute(train, target = character(0), cols = list(AL1 = 0,AL2=0,PM1=imputeMedian(),PM2=imputeMedian(),CM4=0))
summary(imputed$data)
summary(train)
train1<-imputed$data
summary(train1)
str(train1)

##Corelation Study
library(corrplot)
train1numeric<-Filter(is.numeric,train1)
cor1<-cor(train1numeric)
dev.off()
corrplot(cor1,method="number")
train1numeric
cor1
summary(train1)
train1$CM2<-NULL
train1$CM1<-as.factor(train1$CM1)
train1$CM4<-as.factor(train1$CM4)
summary(train1)
View(train1)

##Bivariate Analysis

g1<-ggplot(data = train1) + 
  geom_bar(aes(x = train1$Remarks, fill = train1$CM1, colour = train1$CM1)) +labs(title="Remarks vs CM1")+scale_x_discrete(labels = wrap_format(10))
g2<-ggplot(data = train1) + 
  geom_bar(aes(x = train1$Remarks, fill = train1$CM4, colour = train1$CM4)) +labs(title="Remarks vs CM4")+scale_x_discrete(labels = wrap_format(10))
g2
g1
grid.arrange(g1,g2)

table(train1$Remarks,train1$CM4)
table(train1$Remarks,train1$CM1)

##Model Building
##Split the input data into train and test
dim(train1)
S<-floor(0.70*nrow(train1))
S
nrow(train1)
set.seed(1188)
S2<- sample(c(1:931), size = S)

cell.train <- train1[S2,]
cell.test<- train1[-S2,]
View(cell.train)

dim(cell.train)
dim(cell.test)
t1<-(prop.table(table(cell.train$Remarks))*100)
t1
trainRemarks<-as.data.frame(t1)
table(cell.test$Remarks)
t2<-(prop.table(table(cell.test$Remarks))*100)
testRemarks<-as.data.frame(t2)
trainRemarks
testRemarks


##Convert the First Column name to an R understandable name

colnames(cell.train)[1]<-"DaysKPIFailure"
colnames(cell.test)[1]<-"DaysKPIFailure"


## Defining the train and test tasks using the MLR package

trainTask = makeClassifTask(data = cell.train,target = "Remarks")
testTask = makeClassifTask(data = cell.test, target = "Remarks")

trainTask ## To check the details
testTask
str(getTaskData(trainTask)) ## To check details

## Method 1- Naive Bayes

## SOP
## 1. Make Learner
## 2. Train Learner with Task
## 3. Predict



nb.learner = makeLearner("classif.naiveBayes")

nb.model = train(nb.learner, trainTask)
nb.predict=predict(nb.model,testTask)

##table(nbpredict$data$truth,nbpredict$data$response)
confusionMatrix(nb.predict$data$truth,nb.predict$data$response)


## Method 2: CART

getParamSet("classif.rpart") ## Gets you tunable parameters
cart.learner = makeLearner("classif.rpart", predict.type = "response")
cart.model = train(cart.learner, trainTask)

cartModel=getLearnerModel(cart.model) ## In case you need to plot tree

prp(cartModel,extra=2, roundint=FALSE)## For plotting tree, you may need rpart.plot

#make predictions
cart.predict = predict(cart.model, testTask)

##table(tpmodel$data$truth,tpmodel$data$response)
confusionMatrix(cart.predict$data$truth,cart.predict$data$response)


## Method 3: Random Forest

getParamSet("classif.randomForest")

rf.learner = makeLearner("classif.randomForest", predict.type = "response")


rf.model = train(rf.learner, trainTask)

rf.predict = predict(rf.model, testTask)
rf.predict$data

table1<-table(rf.predict$data$truth,rf.predict$data$response)
View(table1)
confusionMatrix(rf.predict$data$truth,rf.predict$data$response)
table2<-confusionMatrix(rf.predict$data$truth,rf.predict$data$response)

View(table2$table)
table2$overall
##Tunning
rf_param = makeParamSet(
  makeIntegerParam("ntree",lower = 50, upper = 500),
  makeIntegerParam("mtry", lower = 3, upper = 10),
  makeIntegerParam("nodesize", lower = 10, upper = 20))

rancontrol = makeTuneControlRandom(maxit = 50L)
set_cv = makeResampleDesc("CV",iters = 3L)
tuned.rf = tuneParams(learner = rf.learner, resampling = set_cv, task = testTask, par.set = rf_param, control = rancontrol, measures = acc)
#Parameter Output
tuned.rf$y
tuned.rf$x

tuned.rfLearner = setHyperPars(rf.learner, par.vals = tuned.rf$x)
tuned.rfModel = train(tuned.rfLearner, trainTask)
tuned.rfPredict = predict(tuned.rfModel, testTask)
#rf.learner = makeLearner("classif.randomForest", predict.type = "response", par.vals = list(ntree = 400, mtry = 3,nodesize=20))

confusionMatrix(tuned.rfPredict$data$truth, tuned.rfPredict$data$response)


##Validation Data Preparation
validate = read_xlsx(choose.files())
validate_actual<-read_xlsx(choose.files())
str(validate)
validate<-as.data.frame(validate)
str(validate)
summary(validate)
View(validate)
dim(validate)
##Convert the Remarks into Factor
validate$AL1<-as.factor(validate$AL1)
validate$AL2<-as.factor(validate$AL2)
validate$CM1<-as.factor(validate$CM1)

validate$CM4<-as.factor(validate$CM4)
validate$Remarks<-as.factor(validate$Remarks)
#Remove CM3 as it is not adding any value in the equation
validate$CM3<-NULL
##Remove the Cell ID as well as it is not required at this moment
validate$`Cell ID`<-NULL
#validate$Remarks<-NULL
validate$CM2<-NULL

colnames(validate)[1]<-"DaysKPIFailure"
summary(validate)
levels(validate$CM1) <- levels(cell.train$CM1)
levels(validate$CM4)<-levels(cell.train$CM4)
levels(validate$AL1)<-levels(cell.train$AL1)
levels(validate$AL2)<-levels(cell.train$AL2)
levels(validate$Remarks)<-levels(cell.train$Remarks)
imputed_val = impute(validate, target = character(0), cols = list(AL1 = 0, AL2=0,PM1=imputeMedian(),PM2=imputeMedian(),CM4=0,Remarks=0))
validate1<-imputed_val$data
str(validate1)
summary(validate1)


rf.model$learner.model

summary(validate1)
summary(cell.train)

##Predict the ouput of validate using rf.model


output<-predict(rf.model$learner.model,newdata=validate1)

View(output)
summary(output)
#testTask1<-NULL

str(output)

output<-as.data.frame(output)
View(output)
table(output)
##Append the output/remarks to original data
str(validate_actual)

validate_actual<-cbind(validate_actual,output)

View(validate_actual)
validate_actual$Remarks<-NULL
colnames(validate_actual)[14]<-'Remarks'
##Export the output as CSV
getwd()
write.xlsx(validate_actual,file="output.xlsx")
