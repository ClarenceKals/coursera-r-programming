#Q2

library(dplyr)

best <- function(state, outcome) {
        datay <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
        outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23) # to validate outcome
        ifelse(outcome %in% names(outcomes), outcome, stop("invalid outcome")) 
        ifelse(state %in% unlist(distinct(datay[, 7, drop = FALSE])), state, stop("invalid state") ) 
        #validate state: need plyr library for distinct to work
        
        datax <- datay[, c(2, 7, outcomes[outcome])] # 3 colsubset with specified outcome
        arrange.data <- arrange(datax, datax[,3], datax[,1]) # arrange dataframe before creating list
        mysety <- split(arrange.data, arrange.data$State) #split data by State
        subsety <- mysety[[state]] #list of specific state
        remove.y <- na.omit(subsety) # remove rows with NAs
        remove.y[1,1]
}