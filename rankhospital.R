rankhospital <- function(state, outcome, num = "best") {
    #below helps me set the directory. It will normally be commented out.
    directory <- "\\Users\\kcaj2\\Desktop\\Coursera\\R Programming\\Assignment 3"
    setwd(directory)
    
    ## Below reads the data and makes the string "Not Available" equal to NA. It also preserves strings as strings (normally converts to factor)
    rawdata <- data.frame(read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE))
    
    ## Check that state and outcome are valid
    if(is.element(state, state.abb)==FALSE) {
        stop("invalid state")
    }
    if(is.element(outcome, c("heart attack", "heart failure", "pneumonia"))== FALSE) {
        stop("invalid outcome")
    }
    else{
        statedata <- subset.data.frame(rawdata,rawdata$State==state)
        #the below 3 ifs sort data by moratlity rate, then hospital name
        if(outcome == "heart attack"){
            sortdata <- statedata[order(statedata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, statedata$Hospital.Name),]
        }
        if(outcome == "heart failure"){
            sortdata <- statedata[order(statedata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, statedata$Hospital.Name),]
        }
        if(outcome == "pneumonia"){
            sortdata <- statedata[order(statedata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, statedata$Hospital.Name),]
        }
        #below will return the values we are looking for
        if(num == "best") {
            return(sortdata[1,2])
        }
        else if(num == "worst") {
            if(outcome == "heart attack"){
                return(statedata[which.max(statedata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),2])
            }
            if(outcome == "heart failure"){
                return(statedata[which.max(statedata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),2])
            }
            if(outcome == "pneumonia"){
                return(statedata[which.max(statedata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),2])
            }
        }
        else return(sortdata[num, 2])
    }
}