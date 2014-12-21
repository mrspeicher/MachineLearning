## Begin by loading the required packages
## If they are not installed in your system then install them from Cran-R 
## using install.packages('package_name')
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(e1071)
library(randomForest)

## Get the data from the URLs in the assignment and set the NA variables to NA
trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
training <- read.csv(url(trainUrl), na.strings=c("NA","#DIV/0!",""))
testing <- read.csv(url(testUrl), na.strings=c("NA","#DIV/0!",""))

## Particion the training data set for 60% training 40% testing
inTrain <- createDataPartition(y=training$classe, p=0.6, list=FALSE)
myTraining <- training[inTrain, ]; myTesting <- training[-inTrain, ]
dim(myTraining); dim(myTesting)
## You should have 11,776 cases in training and 7,846 in testing

## Clean the data

## Because the file is so big we will look for variables with
## Near-zero variances
myDataNZV <- nearZeroVar(myTraining, saveMetrics=TRUE)

## Take a look
myNZVvars <- names(myTraining) %in% c("new_window", "kurtosis_roll_belt", "kurtosis_picth_belt",
                                      "kurtosis_yaw_belt", "skewness_roll_belt", "skewness_roll_belt.1", "skewness_yaw_belt",
                                      "max_yaw_belt", "min_yaw_belt", "amplitude_yaw_belt", "avg_roll_arm", "stddev_roll_arm",
                                      "var_roll_arm", "avg_pitch_arm", "stddev_pitch_arm", "var_pitch_arm", "avg_yaw_arm",
                                      "stddev_yaw_arm", "var_yaw_arm", "kurtosis_roll_arm", "kurtosis_picth_arm",
                                      "kurtosis_yaw_arm", "skewness_roll_arm", "skewness_pitch_arm", "skewness_yaw_arm",
                                      "max_roll_arm", "min_roll_arm", "min_pitch_arm", "amplitude_roll_arm", "amplitude_pitch_arm",
                                      "kurtosis_roll_dumbbell", "kurtosis_picth_dumbbell", "kurtosis_yaw_dumbbell", "skewness_roll_dumbbell",
                                      "skewness_pitch_dumbbell", "skewness_yaw_dumbbell", "max_yaw_dumbbell", "min_yaw_dumbbell",
                                      "amplitude_yaw_dumbbell", "kurtosis_roll_forearm", "kurtosis_picth_forearm", "kurtosis_yaw_forearm",
                                      "skewness_roll_forearm", "skewness_pitch_forearm", "skewness_yaw_forearm", "max_roll_forearm",
                                      "max_yaw_forearm", "min_roll_forearm", "min_yaw_forearm", "amplitude_roll_forearm",
                                      "amplitude_yaw_forearm", "avg_roll_forearm", "stddev_roll_forearm", "var_roll_forearm",
                                      "avg_pitch_forearm", "stddev_pitch_forearm", "var_pitch_forearm", "avg_yaw_forearm",
                                      "stddev_yaw_forearm", "var_yaw_forearm")
myTraining <- myTraining[!myNZVvars]

## To check the new number of observations
dim(myTraining)
## You should now have 100 variables for each case where before you had 160

## Eliminate the ID numbers as they will mess up the correlations
myTraining <- myTraining[c(-1)]

## Get rid of the variables with too many NAs in the set
trainingV3 <- myTraining #creating another subset to iterate in loop
for(i in 1:length(myTraining)) { #for every column in the training dataset
  if( sum( is.na( myTraining[, i] ) ) /nrow(myTraining) >= .6 ) { #if n?? NAs > 60% of total observations
    for(j in 1:length(trainingV3)) {
      if( length( grep(names(myTraining[i]), names(trainingV3)[j]) ) ==1)  { #if the columns are the same:
        trainingV3 <- trainingV3[ , -j] #Remove that column
      }   
    } 
  }
}

## To check the new N again
dim(trainingV3)
## You should have 58 variables now rather than 60

## We're done cleaning the myTraining training set so let's
## Set it back to the original name and clean up our extra sets:
myTraining <- trainingV3
rm(trainingV3)

## Clean up the other sets using the same procedure
## On the MyTraining testing set and the actual testing set
clean1 <- colnames(myTraining)
clean2 <- colnames(myTraining[, -58]) # already with classe column removed
myTesting <- myTesting[clean1]
testing <- testing[clean2]

## Check the new N
dim(myTesting)
dim(testing)

## Coerce the data to the same type to work with the trees software
for (i in 1:length(testing) ) {
  for(j in 1:length(myTraining)) {
    if( length( grep(names(myTraining[i]), names(testing)[j]) ) ==1)  {
      class(testing[j]) <- class(myTraining[i])
    }      
  }      
}

## And to make sure Coertion really worked, simple smart ass technique:
testing <- rbind(myTraining[2, -58] , testing) #note row 2 does not mean anything, so remove it
testing <- testing[-1,]

## First, use the decision tree algorithm to 
## identify the groupings of teh variables for predictions
modFitA1 <- rpart(classe ~ ., data=myTraining, method="class")

## Take a look at the graph
fancyRpartPlot(modFitA1)

## Now do the predictions on myTesting set
predictionsA1 <- predict(modFitA1, myTesting, type = "class")

## Use confusionMatrix to test the results
confusionMatrix(predictionsA1, myTesting$classe)
## Take a look at your results - overall accuracy should be
## 0.8663 (95% CI 0.08586 - 0.08738)

## Let's compare to another method: Random Forest groupings
modFitB1 <- randomForest(classe ~. , data=myTraining)
predictionsB1 <- predict(modFitB1, myTesting, type = "class")
confusionMatrix(predictionsB1, myTesting$classe)
## Wow! Much better - overall accuracy is
## 0.9985 (95% CI 0.9973, 0.9992)

## So now we work on the actual testing set
predictionsB2 <- predict(modFitB1, testing, type = "class")

## And we do our output files (hoping they are right!)
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predictionsB2)
