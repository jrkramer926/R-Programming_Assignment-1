complete <- function(directory, id = 1:332) {
    #below sets the working directory equal to directory
    #directory <- "\\Users\\kcaj2\\Desktop\\Coursera\\R Programming\\Assignment 1\\specdata\\"
    setwd(directory)
    
    #below makes a vector which will be our first column in our data frame
    id <- c(id)
    
    #below reads the data, finds the number of rows, and concatenates it into a vector
    #append() will add a value to the end of my vector
    
    #below declares our variables that will be used in our loop
    nobs <- c()
    id.csv <- sprintf("%03d.csv", id)
    len.id <- length(id)
    
    #below is the case for one file
    if (min(id) == max(id)) {
        nadata <- read.csv(id.csv)
        completecases <- complete.cases(nadata)
        data <- nadata[completecases,]
        nobs <-append(nobs, nrow(data))
        data.frame(id, nobs)
    }
    #below is the case for multiple files
    else{
        count <- 1
        while(count <= len.id) {
            
            nadata <- read.csv(id.csv[count])
            completecases <- complete.cases(nadata)
            data <- nadata[completecases,]
            
            nobs <-append(nobs, nrow(data))
            count <- count + 1
        }
        data.frame(id, nobs)
        
    }
}