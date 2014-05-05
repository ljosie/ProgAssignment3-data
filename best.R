## dd/mm/yyyy: 04/05/2014 Author: Josie.Tao R Programming on Coursera -- ProgAssignment3

## Finding the best hospital in a state
## Write a function called best that take two arguments: the 2-character abbreviated
## name of a state and an outcome name. The function reads the outcome-of-care-measures.csv 
## file and returns a character vector with the name of the hospital that has the best 
## (i.e. lowest) 30-day mortality for the specified outcome in that state. The hospital name
## is the name provided in the Hospital.Name variable. The outcomes can be one of \heart attack",
## \heart failure", or \pneumonia". Hospitals that do not have data on a particular outcome 
## should be excluded from the set of hospitals when deciding the rankings.

best <- function(state, outcome){
        # Set full path to source file
        setwd("~/Desktop/coursera/ProgAssignment3-data")
        # Read outcome data
        outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        # outcomes[, 11] 
        heartAttack <- as.numeric(outcomes[, 11])
        # outcomes[, 17] 
        heartFailure <- as.numeric(outcomes[, 17])
        # outcomes[, 23] 
        pneumonia <- as.numeric(outcomes[, 23])
        # outcome[, 7]
        stateName <- outcomes[, "State"]
        # outcome[, 2]
        hospitalName <- outcomes[, "Hospital.Name"]
        
        mydata <- data.frame(heartAttack, heartFailure, pneumonia, stateName, hospitalName)
        mydata <- na.omit(mydata)
        mydata <- subset(mydata, stateName == state)
        # Check that state is valid
        if(nrow(subset(mydata, stateName == state)) == 0){
                stop("invalid state")
        }
        # Check that outcome is valid
        if(!is.element(outcome, c("heart attack", "heart failure", "pneumonia"))){
                stop("invalid outcome")
        }
        if(outcome == "heart attack"){
                minHospital <- mydata[which.min(mydata$heartAttack), "hospitalName"]
        }
        if(outcome == "heart failure"){
                minHospital <- mydata[which.min(mydata$heartFailure), "hospitalName"]
        }
        if(outcome == "pneumonia"){
                minHospital <- mydata[which.min(mydata$pneumonia), "hospitalName"]
        }       
        results <- as.character(minHospital)
        # Return hospital name in that state with lowest 30-day death rate
        return (results)
}