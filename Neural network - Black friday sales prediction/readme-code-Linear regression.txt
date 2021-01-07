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



# As these variables are independent of purchase amount . Please ignore these variables

Trainingsample$Product_ID <- NULL
Trainingsample$User_ID <- NULL
Trainingsample$Product_Category_2 <- NULL
Trainingsample$Product_Category_3 <- NULL

Testingsample$User_ID <- NULL
Testingsample$Product_ID <- NULL
Testingsample$Product_Category_2 <- NULL
Testingsample$Product_Category_3 <- NULL

# build the model
modellm <- lm(Purchase ~., Trainingsample)

# predict the values using the model we built
predictionlm <- predict(modellm,Testingsample,interval = 'confidence')

predictionlm

# Plot the predicted amount against product categories.
plotdata <- cbind(Trainingsample, predictionlm)
library("ggplot2")
ggplot(plotdata,aes(Purchase,Product_Category_1))+ geom_point() + stat_smooth(method = "lm", col = "red")

#write the predicted values to CSV file
setwd("~Intro to data science/project")
write.csv(predictionlm,"predictionresult.csv")
