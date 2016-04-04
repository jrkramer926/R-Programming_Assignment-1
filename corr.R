corr <- function(directory, threshold = 0) {
    directory <- "\\Users\\kcaj2\\Desktop\\Coursera\\R Programming\\Assignment 1\\specdata\\"
    setwd(directory)
    
    #below makes the id  # have 3 digits and .csv
    id <- 1:332
    id.csv <- sprintf("%03d.csv", id)
    len.id <- length(id)
    
    
    count <- 1
    cv <- double()
    #below will read each file and combine it into one data set
    while(count <= len.id) {
        nadata <- read.csv(id.csv[count])
        completecases <- complete.cases(nadata)
        data <- nadata[completecases,]
        if(nrow(data) <= threshold){
            v <- double()
        }
        else if(nrow(data) > threshold) {
            v <- cor(data$sulfate, data$nitrate)
        }
        cv <- append(cv,v)
        count <- count + 1
    }
    print(cv)
}