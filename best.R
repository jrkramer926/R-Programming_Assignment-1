best <- function(state, outcome) {
    ##below will read the outcome data
    directory <- "\\Users\\kcaj2\\Desktop\\Coursera\\R Programming\\Assignment 3"
    setwd(directory)
    rawdata <- data.frame(read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE))
    
    ##check that state and outcome are valid
    if(is.element(state, state.abb)== FALSE) {
        stop("invalid state")
    }
    if(is.element(outcome, c("heart attack", "heart failure", "pneumonia"))== FALSE) {
        stop("invalid outcome")
    }
    else {
        #below will sort by state
        statedata <- subset(rawdata, rawdata$State == state)
        ##return hospital name in that state with loswest 30-day death rate
        if(outcome == "heart attack") {
            return(statedata[which.min(statedata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),2])
                    }
        if(outcome == "heart failure") {
            return(statedata[which.min(statedata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),2])
        }
        if(outcome == "pneumonia") {
            return(statedata[which.min(statedata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),2])
        }
    }
}