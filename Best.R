

best <- function(state, outcome) {
  ## Read outcome data## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  Outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  state_hospitals <- subset(Outcome, State == state, select = c(2,7,11,17,23))
  if (nrow(state_hospitals) == 0) {
    message(paste('"Error in best("', state, '", "', outcome, '") : invalid state")'))
    return()
  }
  if ((outcome != 'heart attack') && (outcome != 'heart failure') && (outcome != 'pneumonia') )
  {
    message(paste('"Error in best("', state, '", "', outcome, '") : invalid outcome")'))
    return()    
  }  
  if (outcome == 'heart attack')
  {
    state_hospitals[,3] <- as.numeric(state_hospitals[,3]) 
    top <- state_hospitals[order(state_hospitals[,3], state_hospitals[,1]),]
  }
  if (outcome == 'heart failure')
  {
    state_hospitals[,4] <- as.numeric(state_hospitals[,4]) 
    top <- state_hospitals[order(state_hospitals[,4], state_hospitals[,1]),]
  }
  if (outcome == 'pneumonia')
  {
    state_hospitals[,5] <- as.numeric(state_hospitals[,5]) 
    top <- state_hospitals[order(state_hospitals[,5], state_hospitals[,1]),]
  }  
  top[1,1]   
}
  