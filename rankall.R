#Q4
#library(plyr)

rankall <- function(outcome, num = "best") {
        datay <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
        outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23) # to validate outcome
        ifelse(outcome %in% names(outcomes), outcome, stop("invalid outcome")) 
        
        datax <- (datay[, c(2, 7, outcomes[outcome])]) # 3 colsubset with specified outcome
        arrange.data <- arrange(datax, datax[,2], datax[,3], datax[, 1]) # arrange dataframe before creating list
        mysety1 <- split(arrange.data, arrange.data$State) # split data by State
        mysety <- lapply(mysety1, na.omit) # omit NA's from each list
        
        lapsety <- lapply(mysety, function (x) {
                nums <- c("best" = 1, 2:(nrow(x) - 1), "worst" = nrow(x))
                x[nums[num], 1:2]  #
        }) # list of specified rank
        do.call(rbind, lapsety) # create dataframe from list
}
