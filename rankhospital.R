

rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
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
    state_hospitals2 <- subset(state_hospitals,  1 == 1 , select = c(1,3) )
  }
  if (outcome == 'heart failure')
  {
    state_hospitals2 <- subset(state_hospitals, 1 == 1 , select = c(1,4) )
  }
  if (outcome == 'pneumonia')
  {
    state_hospitals2 <- subset(state_hospitals, 1 == 1  , select = c(1,5) )
  }
  state_hospitals2[,2] <- as.numeric(state_hospitals2[,2])
  state_hospitals3 <- state_hospitals2[complete.cases(state_hospitals2),]
  top <- state_hospitals3[order(state_hospitals3[,2], state_hospitals3[,1]),]
  if (num == "best")
  {
    number <- 1
  }
  else if (num == "worst")
  {
    number <- nrow(top)
  }
  else
  {
    number <- as.integer(num)
  }
  top[number,1]   
}
