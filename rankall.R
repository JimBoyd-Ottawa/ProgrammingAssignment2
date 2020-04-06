

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  Outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  state_hospitals <- subset(Outcome, 1 == 1, select = c(2,7,11,17,23))

  if ((outcome != 'heart attack') && (outcome != 'heart failure') && (outcome != 'pneumonia') )
  {
    message(paste('"Error in rankall("', outcome, '", "', num, '") : invalid outcome")'))
    return()    
  }  
  if (outcome == 'heart attack')
  {
    state_hospitals2 <- subset(state_hospitals,  1 == 1 , select = c(1,2,3) )
  }
  if (outcome == 'heart failure')
  {
    state_hospitals2 <- subset(state_hospitals, 1 == 1 , select = c(1,2,4) )
  }
  if (outcome == 'pneumonia')
  {
    state_hospitals2 <- subset(state_hospitals, 1 == 1 , select = c(1,2,5) )
  }
  state_hospitals2[,3] <- as.numeric(state_hospitals2[,3])
  state_hospitals3 <- state_hospitals2[complete.cases(state_hospitals2),]
## loop on state and create one record per state
  states <- unique(state_hospitals3[,2])
  states2 <- states[order(states)]
  print(paste( "     ", "     Hospital    ", "State"))
  for (i in states2 )
  {
     state_hospitals4 <- subset(state_hospitals3, State == i, select = c(1,2,3))
     top <- state_hospitals4[order(state_hospitals4[,2], state_hospitals4[,3], state_hospitals4[,1]),]
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
     print(paste( i, top[number,1], i))
  }
  return()
}