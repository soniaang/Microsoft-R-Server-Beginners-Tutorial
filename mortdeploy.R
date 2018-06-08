  #Remote Execution and Web Services for R Server
library(mrsdeploy)
dataDir <- "C:\\Users\\sonan\\Documents\\Revolution Demos\\SampleData"
mortdata<- read.csv(file.path(dataDir, "MortgageDefault_Raw.csv"))

# Read from csv file and create your model
#loans <- read.csv("C:\Users\sonan\Documents\Revolution Demos\SampleDatalendingclub/LoanStats.csv")
head(mortdata, 5)

# Train the model
#idx <- runif(nrow(loans)) > 0.75
#train <- loans[idx == FALSE,]

mortDefaultModel <- rxLogit(default~ creditScore + yearsEmploy + ccDebt, 
                         data = mortdata, blocksPerRead = 5)

# Scoring Function
mortPredictService <- function(creditScore, yearsEmploy,ccDebt) {
  inputData <- data.frame(creditScore= creditScore, yearsEmploy= yearsEmploy,ccDebt= ccDebt)
  prediction <- rxPredict(mortDefaultModel, inputData)
  mortScore <- prediction$default_Pred 
}


# Remote Login, a prompt will show up to input user and pwd information
#endpoint <- "http://13.92.186.121:12800"
endpoint <- "http://localhost:12800"

#remoteLogin(endpoint, session = TRUE, diff = TRUE)

#remoteLogin("deployr.eastus.cloudapp.azure.com", username = "admin", password = "Admin1$$",
            #session = TRUE, diff = TRUE, commandline = TRUE)

library(mrsdeploy)
remoteLogin(endpoint, username = "admin", password = "Admin1$$",
session = FALSE, diff = TRUE, commandline = TRUE)

# pseudo `unique` service name so no collision in example
service_name <- paste0("mortPredictService", round(as.numeric(Sys.time()), 0))

# Publish service
api <- publishService(
  service_name,
  code = mortPredictService,
  model = mortDefaultModel,
  inputs = list(creditScore = 'numeric', yearsEmploy= 'numeric',ccDebt= 'numeric'),
  outputs = list(mortScore = 'numeric'),
  v = 'v3.0.0'
)


# Show API capabilities
api$capabilities()

#Consume the service
mortScore <- api$mortPredictService(470, 5, 14094)

mortScore$output("mortScore")

#List all services
services <- listServices()
services

#Generate swagger json file
cat(api$swagger(), file = "C:/Users/sonan/Documents/mortPredict.json")

#Logout
remoteLogout()