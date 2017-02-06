#############################################################################################################
#Define the file path 
# Read the data into a data frame in memory
# Example 1
dataDir <- "C:\\Users\\sonan\\Documents\\Revolution Demos\\SampleData"
mortXdfTrain<- file.path(dataDir,"mortDefaultSmall2008.xdf")
mortXdfTest <-file.path(dataDir,"mortDefaultSmall2009.xdf")
mortDF <- rxDataStep(mortXdfTrain)


#Example 2

bigAirDataDF<- rxXdfToDataFrame(bigAirData)
bigAirData<- file.path(dataDir,"AirlineDataSubsample.xdf")
bigAirDataDF <- rxDataStep(bigAirData,maxRowsByCols = 132118240)


#Example 3
mortXdfTrain <- file.path(rxGetOption("sampleDataDir"), "mortDefaultSmall2008.xdf")
mortXdfTest <- file.path(rxGetOption("sampleDataDir"), "mortDefaultSmall2009.xdf")
dataDir <- "C:\\Program Files\\Microsoft\\R Server\\R_SERVER\\library\\RevoScaleR\\SampleData"



#IMPORT THE DATA
rxImport(inData = "mortDefaultSmall2008.csv", outFile = "mortDefaultSmall2008.xdf", 
                     colClasses = c(creditScore = "integer",
                                    yearsEmploy =  "integer",
                                    ccDebt = "integer",
                                    year = "integer",
                                    default = "factor"),overwrite = TRUE)


###########################################################################
#How to generate PMML code 
#PMML
bigDataDir <- "C:\\Users\\sonan\\Documents\\Revolution Demos\\SampleData"
bigAirData <- file.path(bigDataDir, "AirlineDataSubsample.xdf")
# About 150 million observations
rxLinModObj <- rxLinMod(ArrDelay~Year + DayOfWeek, data = bigAirData,
                        blocksPerRead = 10)

library(pmml)
pmml(as.lm(rxLinModObj))


############################################################################

#RXDATASTEP  EXAMPLE 

#CREATE AN XDF
set.seed(39)
myData <- data.frame(x1 = rnorm(10000), x2 = runif(10000))

rxDataStep(inData = myData, outFile = "testFile.xdf",
           rowSelection = x2 > .1,
           transforms = list( x3 = x1 + x2 ),
           rowsPerRead = 5000 )
rxGetInfo("testFile.xdf")

######################################################################
#CREATE NEW VAR RXDATASTEP

head(mortDF, n=3)

rxGetInfo(mortData, getVarInfo = TRUE, numRows=3)

outfile <- "outfile3.xdf"

mortDataNew <- rxDataStep(
  # Specify the input data set
  inData = mortXdfTrain,
  # Put in a placeholder for an output file
  outFile = outfile,
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
x<-rxDataStep(outfile)
outfile

#################################################################
#    Stepwise Variable Search Example        
#################################################################

system.time(regst1 <- rxLinMod(formula=ArrDelay~DayOfWeek + CRSDepTime, data=bigAirData,
                               variableSelection = rxStepControl(method="stepwise",
                                                                 scope = ~ DayOfWeek + CRSDepTime + Year + Dest + Origin + UniqueCarrier )))

summary(regst1)



#############################################

save ( mode1.file = "model.Rdata")
rm(mod1)
load("model.Rdata")
summary(mod1)


######################################################
#Sorting 
#2 variable sort or more


xdfsorted = tempfile( fileext=".xdf")

rxSort (inData = mortXdfTrain,
          outFile = xdfsorted,
          sortByVars = c("ccDebt","creditScore"),
decreasing = c( TRUE,FALSE),
overwrite= TRUE)

rxDataStep(xdfsorted, numRows= 10)
#######################################################################
##########################################################################
#Cubes
mortCube <- rxCube(~F(creditScore):catDebt, data = mortDataNew)
mortCube


