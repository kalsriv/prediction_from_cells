
library(caret)
library(skimr)
library(mlbench)
library(caretEnsemble)

NKcellDF <- read.csv('/home/srivastava/Documents/NK_Cells.csv')

head(NKcellDF, n=2)
summary(NKcellDF)
skimmed <- skim(NKcellDF)
skimmed

x <- ncol(NKcellDF)
dataset <- NKcellDF[,2:x]

dataset$Status <- factor(dataset$Status)
validation_index <- createDataPartition(dataset$Status,p=0.80, list=FALSE)
# select 20% of the data for validation
testSet<- dataset[-validation_index,]
# use the remaining 80% of data to training and testingthe models
trainSet <- dataset[validation_index,]

levels(factor(dataset$Status))

control <- trainControl(method="cv", number=10)


set.seed(7)
modelSvmradial <- train(Status~., data=dataset, method="svmRadial", trControl=control)
set.seed(7)
modelLvq <- train(Status~., data=dataset, method="lvq", trControl=control)
set.seed(7)
modelRF <- train(Status~., data=dataset, method="rf", trControl=control)
set.seed(7)
modelRda <- train(Status~., data=dataset, method="rda", trControl=control)
set.seed(7)
modelAda <- train(Status~., data=dataset, method="ada", trControl=control)

results <- resamples(list(ADA=modelLAda, LVQ=modelLvq, RF=modelRF, SVM=modelSvmradial, RDA=modelRda))
summary(results)
#generate the numbers from your models
dotplot(results)

predictions <- predict(models$Svm, testSet)
finalMatrix <- confusionMatrix(predictions, factor(testSet$class))
finalMatrix

