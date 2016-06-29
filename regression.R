insurance<- read.csv("insurance.csv", stringsAsFactors = TRUE)
str(insurance)
summary(insurance$charges)
hist(insurance$charges)
table(insurance$region)
cor(insurance[c("age","bmi","children",'charges')])
pairs(insurance[c("age","bmi","children",'charges')])
library(psych)
pairs.panels(insurance[c("age","bmi","children",'charges')])
ins_model<- lm(charges~age+children+bmi+sex+smoker+region,data=insurance)
#ins_model<- lm(charges~., data=insurance)
ins_model
summary(ins_model)
insurance$age2<- insurance$age^2
insurance$bmi30<- ifelse(insurance$bmi>=30,1,0)
#bmi30*smoker=bmi30+smokeryes+bmi30:smokeryes
ins_model2<- lm(charges~age+age2+children+bmi+sex+bmi30*smoker+region,data=insurance)
summary(ins_model2)

####################[part2]
wine<- read.csv("whitewines.csv")
str(wine)
hist(wine$quality)
wine_train<- wine[1:3750,]
wine_test<-  wine[3751:4898,]
library(rpart)
m.rpart<- rpart(quality~., data=wine_train)
m.rpart
library(rpart.plot)
rpart.plot(m.rpart, digits=3)
rpart.plot(m.rpart,digits=4, fallen.leaves=TRUE,type=3,extra=101)
p.rpart<- predict(m.rpart,wine_test)
summary(p.rpart)
summary(wine_test$quality)
cor(p.rpart,wine_test$quality)
MAE<- function(actual,predicted){
  mean(abs(actual-predicted))
}
MAE(p.rpart,wine_test$quality)
mean(wine_train$quality)
###more accurate algorithm; M5P()
library(RWeka)
m.m5p<- M5P(quality~., data=wine_train)
m.m5p
summary(m.m5p)
p.m5p<-predict(m.m5p,wine_test)
summary(p.m5p)
cor(p.m5p,wine_test$quality)
MAE(wine_test$quality,p.m5p)