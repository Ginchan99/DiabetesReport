rm(list=ls()) #clearing the enviornment
dia_test = read.csv("diabetes_test.csv") #load csv file
dia_train = read.csv("diabetes_train.csv") #load csv file

#library(caTools) #load library caTools
#finding column names of train dataset 
colnames(dia_train) 
#creating logistin function
model1 = glm(Outcome ~.,data =dia_train,family = binomial)
#summary of model1
summary(model1)
#Model creation based on dependent variable Outcome
model2 = glm(Outcome ~ Pregnancies+Glucose+BloodPressure+BMI+DiabetesPedigreeFunction,data =dia_train,family = binomial)
summary(model2)#finding summary of model2

pred = predict(model2, type="response", newdata=dia_test)#prediction on model based on test dataset
diapredDF = data.frame(dia_test,pred)#creating data and adding predictions in dataset
diapredDF$diapred = diapredDF$pred>0.5

#confusion matrix -  categorize the predictions against the actual values

# >0.5 will be TRUE and <0.5 will be FALSE 
cf = table(diapredDF$Outcome,diapredDF$diapred)

cf = as.data.frame.matrix(table(diapredDF$Outcome,diapredDF$diapred))# to check if the function is a data frame

View(cf)
accuracy = (96+32)/(96+12+28+32)
Truepositive = (96+32)/(96+12+28+32)
Truenegative = (32)/(96+12+28+32)
Falsepositive = (12)/(96+12+28+32) 
Falsenegative = (28)/(96+12+28+32) 