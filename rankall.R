## dd/mm/yyyy: 04/05/2014 Author: Josie.Tao R Programming on Coursera -- ProgAssignment3

## Write a function called rankall that takes two arguments: an outcome name (outcome) and a hospital rank-
## ing (num). The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
## containing the hospital in each state that has the ranking specified in num. For example the function call
## rankall("heart attack", "best") would return a data frame containing the names of the hospitals that
## are the best in their respective states for 30-day heart attack death rates. The function should return a value
## for every state (some may be NA). The first column in the data frame is named hospital, which contains
## the hospital name, and the second column is named state, which contains the 2-character abbreviation for
## the state name. Hospitals that do not have data on a particular outcome should be excluded from the set of
## hospitals when deciding the rankings.

rankall <- function(outcome, num = "best"){
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
        state <- outcomes[, "State"]
        # outcome[, 2]
        hospital <- outcomes[, "Hospital.Name"]
        
        mydata <- data.frame(heartAttack, heartFailure, pneumonia, state, hospital)
        # split data.frame by "state"
        stateList <- as.character(sort(unique(mydata$state)))

        # Check that outcome is valid
        if(!is.element(outcome, c("heart attack", "heart failure", "pneumonia"))){
                stop("invalid outcome")
        }
        
        if(num == "best"){
                num <- 1
        }
        
        if(outcome == "heart attack"){
                Hospital.Name <- c()
                State.Name <- c()
                # Every "state" in stateList, get rankhospital value
                for(i in 1:length(stateList)){
        
                        mydataSplit <- subset(mydata, state == stateList[i])
                        # num == "worst" means num can be different when mydataSplit is ordered by "heart attack"
                        if(num == "worst"){
                                boolVal <- is.na(mydataSplit$heartAttack)
                                # id is exclusive NA
                                id <- nrow(mydataSplit[!boolVal,])
                                # rankhospital by "heart attack"
                                Hospital.Name[i] <- as.character(mydataSplit[order(mydataSplit$heartAttack, mydataSplit$hospital)[id], "hospital"])
                                State.Name[i] <- stateList[i]
                        }else{
                        # num == "best" && num == numbers can use num as index directory.
                        Hospital.Name[i] <- as.character(mydataSplit[order(mydataSplit$heartAttack, mydataSplit$hospital)[num], "hospital"])
                        State.Name[i] <- stateList[i]
                        }
                }

        }
        if(outcome == "heart failure"){
                Hospital.Name <- c()
                State.Name <- c()
                for(i in 1:length(stateList)){
                        mydataSplit <- subset(mydata, state == stateList[i])
                        # num == "worst" means num can be different when mydataSplit is ordered by "heart attack"
                        if(num == "worst"){
                                boolVal <- is.na(mydataSplit$heartFailure)
                                # id is exclusive NA
                                id <- nrow(mydataSplit[!boolVal,])
                                #rankhospital by "heart failure"
                                Hospital.Name[i] <- as.character(mydataSplit[order(mydataSplit$heartFailure, mydataSplit$hospital)[id], "hospital"])
                                State.Name[i] <- stateList[i]
                        }else{
                        # num == "best" && num == numbers can use num as index directory.
                        Hospital.Name[i] <- as.character(mydataSplit[order(mydataSplit$heartFailure, mydataSplit$hospital)[num], "hospital"])
                        State.Name[i] <- stateList[i]
                        }
                }
                
        }
        if(outcome == "pneumonia"){
                Hospital.Name <- c()
                State.Name <- c()
                for(i in 1:length(stateList)){
                        mydataSplit <- subset(mydata, state == stateList[i])
                        # num == "worst" means num can be different when mydataSplit is ordered by "heart attack"
                        if(num == "worst"){
                                boolVal <- is.na(mydataSplit$pneumonia)
                                id <- nrow(mydataSplit[!boolVal,])
                                # rankhospital by "pneumonia"
                                Hospital.Name[i] <- as.character(mydataSplit[order(mydataSplit$pneumonia, mydataSplit$hospital)[id], "hospital"])
                                State.Name[i] <- stateList[i]
                        }else{
                        # num == "best" && num == numbers can use num as index directory.
                        Hospital.Name[i] <- as.character(mydataSplit[order(mydataSplit$pneumonia, mydataSplit$hospital)[num], "hospital"])
                        State.Name[i] <- stateList[i]
                        }
                }
                
        }       
        # Return hospital name in that state with lowest 30-day death rate
        results <- data.frame(hospital = Hospital.Name, state = State.Name)
        return (results)          
}