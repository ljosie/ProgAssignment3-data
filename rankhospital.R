## dd/mm/yyyy: 04/05/2014 Author: Josie.Tao R Programming on Coursera -- ProgAssignment3

## Ranking hospitals by outcome in a state
## Write a function called rankhospital that takes three arguments: the 2-character abbreviated
## name of a state(state), an outcome(outcome), and the ranking of a hospital in that state for
## that outcome(num). The function reads the outcome-of-care-measures.csv file and returns a ch-
## aracter vector with the name of the hospital that has the ranking specified by the num argument. 

rankhospital <- function(state, outcome, num = "best"){
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
        
        if(num == "best"){
                num <- 1
        }
        if(outcome == "heart attack"){
                if(num == "worst"){
                        num <- nrow(mydata)
                }
                Hospital.Name <- mydata[order(mydata$heartAttack, mydata$hospitalName)[1:num], "hospitalName"]
        }
        if(outcome == "heart failure"){
                if(num == "worst"){
                        num <- nrow(mydata)
                }
                Hospital.Name <- mydata[order(mydata$heartFailure, mydata$hospitalName)[1:num], "hospitalName"]
                
        }
        if(outcome == "pneumonia"){
                if(num == "worst"){
                        num <- nrow(mydata)
                }
                Hospital.Name <- mydata[order(mydata$pneumonia, mydata$hospitalName), "hospitalName"]
                
        }       
        results <- as.character(Hospital.Name[num])
        # Return hospital name in that state with lowest 30-day death rate
        return (results)        
}