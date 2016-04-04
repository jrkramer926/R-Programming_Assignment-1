rankall <- function(outcome, num = "best") {
    ##Read outcome data
    directory <- "\\Users\\kcaj2\\Desktop\\Coursera\\R Programming\\Assignment 3"
    setwd(directory)
    
    #below stores the column numbers for each outcome
    outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    
    rawdata <- data.frame(read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE))
    mycol <- c(2,7,outcomes[outcome])
    outcomedata <- rawdata[mycol]
    orderdata <- outcomedata[order(outcomedata[,2], outcomedata[,3], outcomedata[,1]),]
    
    
    if(is.element(outcome, c("heart attack", "heart failure", "pneumonia"))== FALSE) {
        stop("invalid outcome")
    }
    else {
        if(num == "worst") {
            orderdata <- orderdata[complete.cases(orderdata),]
            returnframe <- aggregate(orderdata, list(orderdata$State), function(x) x[length(x)])
            returnframe <- returnframe[2:1]
            row.names(returnframe) <- returnframe$Group.1
            names(returnframe) <- c("Hospital", "State")
            
            returnframe
            }
        else {
            if(num == "best") num = 1
            
            splitdata <- split(orderdata, orderdata$State)
            returnframe <- data.frame(sapply(splitdata, function(x) x[num,1]))
            returnframe[2] <- data.frame(sapply(splitdata, function(x) x[num,2]))
            names(returnframe) <- c("Hospital", "State") 
            
            returnframe
        }
    }
}
