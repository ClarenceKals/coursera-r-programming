#Week 2 Assignment 1

## Q1 breakdown in bits that worked

files_list <- list.files("./specdata", full.names=TRUE) # creates a list of all the data
y <- rbind(read.csv(files_list[70]), read.csv(files_list[72])) # creates a dataframe 
subsety <- y[ , "nitrate"] # creates a vector of pollutant specified
complete_subsety <- subsety[complete.cases(subsety)] #creates vector with complete cases
mean(complete_subsety) #gives the required mean

## now time for actual function: Q1. below worked well

pollutantmean <- function(directory = "specdata", pollutant, id = 1:332) {
       file_list <- list.files(paste('./',directory, sep =""), full.names = TRUE)[id[1]:id[length(id)]] 
        y <- lapply(file_list, function(x) {read.csv(x)[[pollutant]]}) #produces list with specified pollutant 
        unlisty <- unlist(y) #gives u a vector 
        complete_subsety <- unlisty[complete.cases(unlisty)] #takes out missing values
        mean(complete_subsety)        
}

#Alternative:Q1 works great as well
pollutantmean <- function(directory, pollutant, id = 1:332) {
        file_list <- list.files(paste('./',directory, sep =""), full.names = TRUE)[id[1]:id[length(id)]] 
        dat <- data.frame()     #creates an empty data frame
        for (i in seq_along(id)) {                                
                #loops through the files, rbinding them together into one dataframe 
                dat <- rbind(dat, read.csv(file_list[i]))
        }
        subsety <- dat[ , pollutant] #creates dataframe for selected pollutant
        complete_subsety <- subsety[complete.cases(subsety)] # complete cases
        mean(complete_subsety)        
}

# Q2 
complete <- function(directory = "specdata", id = 1:332) {
        file_list <- list.files(paste('./',directory, sep =""), full.names = TRUE)[id[seq_along(id)]] #list of selected id
        read_y <- lapply(file_list, read.csv) #reads each file
        nobs_complist <- lapply(read_y, function(x) {length(x[complete.cases(x),4])}) # list of complete cases
        nobs_complete <- unlist(nobs_complist)  #vector of length of completed cases
        
        data.frame(id, nobs_complete)
}


# Q3 note: for both sulfate & nitrate

#.............................................................................
corr <- function(directory = "specdata", threshold = 0) {
        file_list <- list.files(directory, full.names = TRUE) #list of all monitors
        read_y <- lapply(file_list, read.csv) #reads each file
        nobs_complist <- lapply(read_y, function(x) {length(x[complete.cases(x),4])}) # list of complete cases
        nobs_complete <- unlist(nobs_complist)  #vecttor of length of completed cases
        subset_y <- lapply(read_y, function(x) {x[complete.cases(x), ] } ) # complete cases list for each monitor
        subset_sulfate <- lapply(subset_y, function(x) {subset(x, select = sulfate) })
        subset_nitrate <- lapply(subset_y, function(x) {subset(x, select =nitrate) })
        
        dat = as.numeric(vector())
        for(i in 1:length(nobs_complete)) {
                
                if(nobs_complete[i] > threshold) {
                        dat <- c(dat, cor(as.numeric(unlist(subset_nitrate[[i]])), as.numeric(unlist(subset_sulfate[[i]])))) 
                }
        }
        dat
}
