############################################################################################
## filenames, location
# AirlineData87to08.csv - large 13G file (xdf), 120M rows
# AirlineDataSubsample.xdf - Medium file  12 M rown, 381 MB (xdf)
#
# Each algorith is written 2x, to show ScaleR vs Open Source R code
#
#First step - use the AirlineDataSubsample.xdf - Medium file  12 M rown, 381 MB (xdf) 
#
#############################################################################################

dataDir <- "C:\\Users\\sonan\\Documents\\Revolution Demos\\SampleData"


rxXdfToText(xdfFile, outFile = "AirlineData87to08.csv", sep = ",")
rxImport(inData = "AirlineDemoSmall.csv", outFile = "myAirlineXdf.xdf", overwrite = TRUE)
xdfFile2

xdfFile2 <- file.path(dataDir,"AirlineDemoSmall")
xdfFile <- file.path(dataDir,"AirlineDataSubsample.xdf")

csvfFile<- read.csv(file.path(dataDir, "AirlineDataSubsample.csv"))

csvfFile<- read.csv(file.path(dataDir, "AirlineDataSubsample.csv"), header=TRUE, 
                    colClasses = c( "integer",
                                    "numeric",
                                    "factor",
                                    "integer", 
                                    "factor",
                                    "factor", 
                                    "factor", 
                                    "factor"))

csvfFile$ArrDelay[is.na(csvfFile$ArrDelay)] <-0
csvfFile$DepDelay[is.na(csvfFile$DepDelay)] <-0
csvfFile$DepDelay[is.na(csvfFile$CRSDepTime)]<-0

##############################################################################################
#  Understand the Metadata
##############################################################################################
rxGetInfo(xdfFile, getVarInfo=TRUE, numRows=5)
system.time(rxGetInfo(xdfFile, getVarInfo=TRUE, numRows=5))
            
rxSummary(~., data=xdfFile)  
rxSummary(~ArrDelay+DepDelay, data = xdfFile)
rxSummary(formula=~ArrDelay:DayOfWeek, data = xdfFile)
summary(csvfFile)           
system.time(summary(csvfFile))
###############################################################################################
# A histogram
###############################################################################################
rxHistogram(~DepDelay,data=xdfFile)
system.time(histogram(~DepDelay,data=csvfFile))

rxHistogram(~DepDelay|DayOfWeek,data=xdfFile)
system.time(histogram(~DepDelay|DayOfWeek,data=csvfFile))

############################################################################################
#############################################################################################
## a linear regression:
#############################################################################################

system.time(reg1 <- rxLinMod(formula=ArrDelay~DepDelay + DayOfWeek + UniqueCarrier,data=xdfFile))
summary(reg1)
reg1

system.time(reg2 <- lm(formula=ArrDelay~DepDelay + DayOfWeek + UniqueCarrier, data=csvfFile))
summary(reg2)
reg2
##############################################################################################
# Decision Tree
##############################################################################################

system.time(tr1 <- rxDTree(formula=ArrDelay~DepDelay + DayOfWeek + UniqueCarrier,maxDepth = 3,data=xdfFile))
summary(tr1)
tr1

#PLOT TREE
library(RevoTreeView)
plot(createTreeView(tr1))

system.time(tr2 <- rpart(formula=ArrDelay~DepDelay + DayOfWeek + UniqueCarrier,data=csvfFile))
summary(tr2)
tr2
#############################################################################################

#Score The model
#############################################################################################
outData='xdfFile_predt2.xdf'
system.time(tr2 <- rxDTree(formula=ArrDelay~CRSDepTime + DayOfWeek,data=xdfFile2))
xdfFile_predt1 <- file.path(dataDir,"AirlineDemoSmall.xdf")
rxGetInfo(xdfFile_predt1,getVarInfo=TRUE)
dfPred <- rxPredict(tr2, data=xdfFile_predt1,outData=xdfFile_predt1, computeResiduals=TRUE)
head(dfPred)
rxSummary(~ArrDelay_Resid,data=dfPred)
rxHistogram(~ArrDelay_Resid ,data=dfPred,xAxisMinMax=c(-250, 300), numBreaks=500,xNumTicks=10)

###############################################################################################
# Plot the model
library(rpart)
library(rpart.plot)
prp(rxAddInheritance(tr1))
prp(rtr1)
###############################################################################################

## Generalized linear model:

###############################################################################################
system.time(glm1 <- rxGlm(formula=ArrDelay~CRSDepTime + DayOfWeek, data=xdfFile))
summary(glm1)
glm1

system.time(glm2 <-  glm(formula=ArrDelay~CRSDepTime + DayOfWeek, data=csvfFile))
summary(glm2)
glm2
##############################################################################################

Repeat the scripts above with the 123 M row file in ScaleR algorithms so it finishes

##############################################################################################

## A Gradient Boosted Decision Trees model

system.time(gbm1 <- rxBTrees(formula=ArrDelay~DepDelay + DayOfWeek + UniqueCarrier,nTree = 5,data=xdfFile))
summary(gbm1)
gbm1
######################################################################################


#BAYES CLASSIFIER

rxGetInfo(mortDF, getVarInfo = TRUE)

mortNB <- rxNaiveBayes(default ~ year + creditScore + yearsEmploy + ccDebt, data = mortXdfTrain)
mortNB
mortNBPred <- rxPredict(mortNB, data = mortXdfTest)

results <- table(mortNBPred[["default_Pred"]], rxDataStep(mortXdfTest,
                                                          maxRowsByCols=6000000)[["default"]])
results

##################################################################################
#CLUSTERING                                                                                               

kclusts1 <- rxKmeans(formula= ~ArrDelay + CRSDepTime, data = bigAirDataDF, outFile = AirlineDataCluster.xdf, numClusters=3)
kclusts1

clust1Lm <- rxLinMod(ArrDelay ~ DayOfWeek, "AirlineDataClusterVars.xdf",
                     rowSelection = .rxCluster == 1 )
clust5Lm <- rxLinMod(ArrDelay ~ DayOfWeek, "AirlineDataClusterVars.xdf",
                     rowSelection = .rxCluster == 5)
summary(clust1Lm)
summary(clust5Lm)

##########################################################################################
#NEURAL NET NOTE THIS CANNOT WORK HERE ! V9 ONLY

binaryFormula <- default ~ creditScore + houseAge + yearsEmploy +
  ccDebt + year

neuralNetModel <- mxNeuralNet(binaryFormula, data = mortXdfTrain, 
                              numHiddenNodes = 10) 
neuralNetScore <- mxPredict(neuralNetModel, data = mortXdfTest, 
                            extraVarsToWrite = "default") 

rxRocCurve(actualVarName = "default", predVarNames = "Probability", 
           data = neuralNetScore, title = "ROC Curve for 'default' using mxNeuralNet") 


#Show how you can find current compute contect
# show how you can do in-database computations inside SQLServer 
rxGetComputeContext()
connectionString <- "Driver=SQL Server;Server=SONIAANG-PRO3\\SQLSERVER2;Database=RevoTestDB;Uid=sonia;Pwd=Welcome@11"
airData <- RxSqlServerData(
  connectionString = connectionString,
  table = "AirlineDemoSmall",
  colInfo = list( ArrDelay = list(type = "integer"), DayOfWeek = list(type = "factor", levels
                                                                      = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))
)
cc <- RxInSqlServer(connectionString = connectionString, autoCleanup = FALSE,
                    consoleOutput = TRUE)
rxSetComputeContext(cc)
rxSummary(~ArrDelay + DayOfWeek, data = airData)
rxGetInfo(data = airData)
head(airData)
mod <- rxLinMod(ArrDelay~CRSDepTime + DayOfWeek, data = airData)
mod

#rxLocalSeq" or "local", "RxLocalParallel" or "localpar", "RxHpcServer" or "mshpc",
"RxHadoopMR" or "hadoopmr", "RxInTeradata" or "teradata", and "RxForeachDoPar" or "do par".


