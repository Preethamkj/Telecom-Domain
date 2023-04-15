counter<-read_xlsx("path")
View(counter)
dim(counter)
#reg.learner = makeLearner("regr.regression")
library(psych)
library(lmtest)
library(zoo)

#Split of train and test data
dim(counter)
S<-floor(0.70*nrow(counter))
S
nrow(reg)
set.seed(1188)
S2<- sample(c(1:394), size = S)

counter.train <- counter[S2,]
counter.test<- counter[-S2,]
View(counter.train)

dim(counter.train)
dim(counter.test)


attach(reg)
reg1 <- lm(C56~.,data=counter.train)
#reg2<-lm(C56~)
anova(reg1)
summary(reg1)
dwtest(reg1)
gqtest(reg1)

#summary(reg1)

#rule1
reg2<-lm(C56~C05+C19 + C33 + C46 + C51 + C64 + C66 + 
           C74 + C94 +C101 + C106 + C111 + C149 + C196+C155,data=counter.train)

#rule1
reg2<-lm(C56~C05+C19 + C33 + C51 + C64 + C66 + 
           C74 + C94 +C101 + C106 + C111 + C149 + C196+C155,data=counter.train)
#rule2
reg2<-lm(C56~C01+C03+C05+ C33 + C47 +C64 + C65 + 
           C91 + C93 + C100 +C106 + 
           C137 + C155 + C196,data=counter.train)

reg2<-lm(C56~C01+C47 +C64 + 
           C91 + C93 + C100 +C106 +C124 + 
           C137 + C155 + C196,data=reg)
reg2<-lm(C56~C03+C05+C18+C47+C33+C58+C74+C124,data=reg)

reg2<-lm(C56~C02+C155+C17+C22+C08+C10+C100+C124+C47,data=reg)
reg2<-lm(C56~C155+C124+C24,data=counter.train)
anova(reg2)
summary(reg2)
dwtest(reg2)
gqtest(reg2)

Prediction=predict(reg2)
Actual=counter.train$C56
#View(Actual)
dev.off()
par(mfrow=c(1,2))

plot(Actual,col="Red",xlab="Data Point")
lines(Actual,col="Red")
plot(Prediction,col="Blue",xlab="Data Point")
lines(Prediction,col="Blue")
backtrack=data.frame(Actual,Prediction)
backtrack$error<-(backtrack$Actual)-(backtrack$Prediction)
backtrack$error2<-(backtrack$error)^2
mean(backtrack$error2)

library(car)
vif(reg2)

library(corrplot)
corrplot(cor(reg))
cor(reg)
corr<-cor(reg)
View(corr)
write.csv(corr,"corr11.csv")
getwd()



##Extract the fitted values and residual values from the reg3 output

fit2 <- fitted(reg2)
res2 <- residuals(reg2)


##Merge the fitted and residual values with Consumer dataset for comparison sake
residualfit <- cbind(counter.train, fit2, res2)
#data<-cbind(reg,backtrack)
#str(data)
#write.csv(data,"data.csv")

##Plot the actual versus fitted values in a plot
with(residualfit, plot(fit2,res2, pch=19, cex=0.6))
abline(a=0,b=0)

##Predict

predict.lm(reg2,data=counter.test)
pred2<-predict.lm(reg1,newdata=counter.test)
View(pred2)
backtrack_1=data.frame(counter.test$C56,pred2)
View(backtrack_1)
dim(counter.test)

View(pred)

Actual1<-counter.test$C56
dev.off()
par(mfrow=c(1,2))
plot(Actual1,col="Red",xlab="Data Point")
lines(Actual1,col="Red")
plot(pred2,col="Blue",xlab="Data Point")
lines(pred2,col="Blue")

