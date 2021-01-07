#Load training and testing data sets.
Training <- read.csv(file.choose(),header=T)
Testing <- read.csv(file.choose(),header=T)

#Take sample values from these data sets as these data sets have more than 5 lakh records

Trainingsample <- Training[sample(1:nrow(Training), 5000, replace=FALSE),]
Testingsample <- Testing[sample(1:nrow(Testing), 5000, replace=FALSE),]

# Make the variables compatable with the model we selected. convert them to numeric

Trainingsample$Gender <- as.numeric(Trainingsample$Gender)
Trainingsample$Age <- as.numeric(Trainingsample$Age)
Trainingsample$Occupation <- as.numeric(Trainingsample$Occupation)
Trainingsample$Stay_In_Current_City_Years <- as.numeric(Trainingsample$Stay_In_Current_City_Years)
Trainingsample$Marital_Status <- as.numeric(Trainingsample$Marital_Status)
Trainingsample$City_Category <- as.numeric(Trainingsample$City_Category)
Trainingsample$Product_Category_1 <- as.numeric(Trainingsample$Product_Category_1)

 
Testingsample$Gender <- as.numeric(Testingsample$Gender)
Testingsample$Age <- as.numeric(Testingsample$Age)
Testingsample$Occupation <- as.numeric(Testingsample$Occupation)
Testingsample$Stay_In_Current_City_Years <- as.numeric(Testingsample$Stay_In_Current_City_Years)
Testingsample$Marital_Status <- as.numeric(Testingsample$Marital_Status)
Testingsample$Product_Category_1 <- as.numeric(Testingsample$Product_Category_1)
Testingsample$City_Category <- as.numeric(Testingsample$City_Category)



# As these variables are independent of purchase amount . Please ignore these variables from model creation

Trainingsample$Product_ID <- NULL
Trainingsample$User_ID <- NULL
Trainingsample$Product_Category_2 <- NULL
Trainingsample$Product_Category_3 <- NULL

Testingsample$User_ID <- NULL
Testingsample$Product_ID <- NULL
Testingsample$Product_Category_2 <- NULL
Testingsample$Product_Category_3 <- NULL

#Normalize the data for adjusting to a common scale so as to accurately compare predicted and actual values. 

normalize <- function(x) {
     return ((x - min(x)) / (max(x) - min(x)))
 }
testingNormlize <- as.data.frame(lapply(Testingsample, normalize))
trainingNormlize <- as.data.frame(lapply(Trainingsample, normalize))

# Load library and build the model
library(neuralnet)
nn <- neuralnet(Purchase ~ ., data=trainingNormlize, hidden=c(2,1), linear.output=FALSE, threshold=0.01)

# check the model results
nn$result.matrix
# plot the model
plot(nn)

# Predict the values for tetsing data set and compare with traning data set
nn.results <- compute(nn, testingNormlize)
results <- data.frame(actual = trainingNormlize$Purchase, prediction = nn.results$net.result)
results

# create a confusion matrix
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)