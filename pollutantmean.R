pollutantmean <- function(directory, pollutant, id = 1:332) {
    #below sets the Work Directory to what the user inputs
    #directory <- "\\Users\\kcaj2\\Desktop\\Coursera\\R Programming\\Assignment 1\\specdata\\"
    setwd(directory)
    
    #below makes the id  # have 3 digits and .csv
    
    id.csv <- sprintf("%03d.csv", id)
    
    #below will read each file and combine it into one data set
    len.id <- length(id)
    count <- 1
    if (min(id) == max(id)) {
        data <- read.csv(id.csv)
    }
    else{
        data <- read.csv(id.csv[1])
        while(count < len.id) {
            data <- rbind(data,read.csv(id.csv[count + 1]))
            count <- count + 1
        }
    }
    #below takes the mean of the id'd pollutant column
    
    if(as.character(pollutant) == "sulfate") {
        mean(data$sulfate, na.rm = TRUE)
    }
    
    else if(as.character(pollutant) == "nitrate") {
        mean(data$nitrate, na.rm = TRUE)
    }
    
    else print("Unrecognized pollutant")
}