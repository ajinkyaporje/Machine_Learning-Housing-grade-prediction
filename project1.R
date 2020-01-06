#Input dataset for analysis
training_data<-read.csv(file.choose(),header = T,na.strings = c("","NA"))
testing_data<-read.csv(file.choose(),header=T,na.strings=c("","NA"))
sum(is.na(training_data))
sum(is.na(testing_data))
summary(training_data)
str(training_data)

testing_data$EXPECTED<-NULL
training_data$EXPECTED<-NULL

#cleaning train data using kNN imputation
library(VIM)
clean_train<-kNN(training_data,k=sqrt(nrow(training_data)))
train<-clean_train[,1:14]

#Roof generated 4 levels after imputation, converting it to 2 levels
train$roof=ifelse(train$roof=="NO",0,1)
train$roof = ifelse(train$roof == "0","no","yes")
train$roof=factor(train$roof)

#cleaning test data using kNN imputation and generating only 2 levels for roof parameter
clean_test<-kNN(testing_data,k=sqrt(nrow(testing_data)))
test<-clean_test[,1:13]
test$roof=ifelse(test$roof=="NO",0,1)
test$roof = ifelse(test$roof == "0","no","yes")
test$roof=factor(test$roof)

#importing libraries for svm
library(e1071)
library(caret)

#creating SVM Model on train data
model_svm<-svm(formula=Grade~.,data=train, type='C-classification',kerner='linear')
summary(model_svm)
a<-fitted(model_svm)
cm<-confusionMatrix(train$Grade,a);cm

#predicting on test data using the model created
test$Grade<-predict(model_svm,test)
summary(test$Grade)

write.csv(test,"F:/test.csv",row.names = F)
