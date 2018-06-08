###############################
#Mortgage Demo Application 
################################
#Initialize Working Directory
setwd("C:\\Users\\sonan\\Documents\\Revolution Demos\\SampleData")
## filenames, location, etc. 
dataDir <- "C:\\Users\\sonan\\Documents\\Revolution Demos\\SampleData"

#############################################################################
#Data Ingestion and Preparation
#############################################################################

inputFile <- file.path(dataDir,  "MortgageDefault_Raw.csv")
mortXdf<- file.path(dataDir, "mortDefaultSmall2.xdf")

# Import the data
mortData=rxImport( inData = inputFile, outFile = mortXdf, overwrite = TRUE)
mortData <- RxXdfData(mortData)
head(mortData)
# Look at summary information 
rxGetInfo( data = mortData, getVarInfo = TRUE)


outFile <- NULL
outFile2 <-NULL
##############################################################################
#Data Exploration 
##############################################################################

nrow(mortData)
ncol(mortData)
names(mortData)


head(mortData, n = 3)

rxGetInfo(mortData, getVarInfo = TRUE, numRows=3)

mortDataNew <- rxDataStep(
  # Specify the input data set
  inData = mortData,
  # Put in a placeholder for an output file
  outFile = outFile2,
  # Specify any variables to keep or drop
  varsToDrop = c("year"),
  # Specify rows to select
  rowSelection = creditScore < 850,
  # Specify a list of new variables to create
  transforms = list(
    catDebt = cut(ccDebt, breaks = c(0, 6500, 13000),
                  labels = c("Low Debt", "High Debt")),
    lowScore = creditScore < 625))

rxGetVarInfo(mortDataNew)
#############################################################################################
rxHistogram(~creditScore, data = mortDataNew )
#############################################################################################
mortCube <- rxCube(~F(creditScore):catDebt, data = mortDataNew)
#############################################################################################
rxLinePlot(Counts~creditScore|catDebt, data=rxResultsDF(mortCube))
#############################################################################################



#############################################################################################
#Model Creation and Analysis
#############################################################################################
myLogit <- rxLogit(default~ccDebt+yearsEmploy , data=mortDataNew)
summary(myLogit)

# Using mortDefaultSmall for predictions and an ROC curve

#mortXdf <- file.path(rxGetOption("sampleDataDir"), "mortDefaultSmall")
mortXdf<- file.path(dataDir, "mortDefaultSmall2.xdf")
#mortData2 <- RxXdfData(mortXdf)


rxGetVarInfo(mortXdf)

logitOut1 <- rxLogit(default ~ creditScore + yearsEmploy + ccDebt, 
                     data = mortXdf,	blocksPerRead = 5)
logitOut1
predFile <- "dummy.xdf"

###dummry
if(file.exists(predFile)) file.remove(predFile)
predOutXdf <- rxPredict(modelObject = logitOut1, data = mortXdf, 
                        writeModelVars = TRUE, predVarNames = "Model1", outData = predFile)
head(predOutXdf)
rxGetInfo(predFile, getVarInfo=TRUE, numRows=5)

xdfFile_pred <- file.path(dataDir,"predFile.xdf")

# Estimate a second model without ccDebt
logitOut2 <- rxLogit(default ~ creditScore + yearsEmploy, 
                     data = predOutXdf, blocksPerRead = 5)

# Add preditions to prediction data file
predOutXdf <- rxPredict(modelObject = logitOut2, data = predOutXdf,writeModelVars = TRUE, predVarNames = "Model2",outData = predFile)
rocOut <- rxRoc(actualVarName = "default", 
                predVarNames = c("Model1", "Model2"), 
                data = predOutXdf)
rocOut
plot(rocOut)

#############################################################################################################################
# Evaluate the model 
###################################################################################################
MortdataF<- read.csv(file.path(dataDir, "MortgageDefaultScored.csv"))
#MortdataF <- file.path(dataDir,  "MortgageDefaultScored.csv")
mortDF <- as.data.frame(MortdataF)
#mortDataF <- RxXdfData(mortDF)
head(MortdataF)
#data(mortDF)


n = nrow(mortDF)
trainIndex = sample(1:n, size = round(0.7*n), replace=FALSE)
train = mortDF[trainIndex ,]
test = mortDF[-trainIndex ,]


columnNames <- rxGetVarNames(mortDF)
model_logistic <- rxLogit(default ~ creditScore + yearsEmploy + ccDebt, 
                          data = train,	blocksPerRead = 5)
#train logistic regression model
#model_logistic <- rxLogisticRegression(modelFormula, trainXdf, type='multiClass', mlTransforms = ft)

#get the accuracy of logistic regression model on training and testing data
score_logistic_train <- rxPredict(model_logistic, train, extraVarsToWrite = columnNames)
score_logistic_test <- rxPredict(model_logistic, test, extraVarsToWrite = columnNames)
head(score_logistic_test)
head(score_logistic_train)
result_logistic_train <- rxCrossTabs(~F(default):F(Scored.Labels), score_logistic_train, returnXtabs = TRUE)
accuracy_logistic_train <- sum(diag(result_logistic_train))/sum(result_logistic_train)
result_logistic_test <- rxCrossTabs(~F(default):F(Scored.Labels), score_logistic_test, returnXtabs = TRUE)
accuracy_logistic_test <- sum(diag(result_logistic_test))/sum(result_logistic_test)

result_logistic_train
accuracy_logistic_train
result_logistic_test
accuracy_logistic_test
