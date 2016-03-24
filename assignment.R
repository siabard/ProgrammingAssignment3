outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
#head(outcome)

best <- function(state, outcome_name) {
  # we'll use global outcome
  # outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  #
  
  if( outcome_name != "heart attack" && outcome_name != "heart failure" && outcome_name != "pneumonia") {
    stop("invalid outcome")
  }
  
  if (nrow(outcome[outcome$State == state,]) == 0) {
    stop("invalid state")
  }
  
    
    outcome_state <- outcome[outcome$State == state,]
    outcome_state[,11] <- as.numeric(outcome_state[,11])
    outcome_state[,17] <- as.numeric(outcome_state[,17])
    outcome_state[,23] <- as.numeric(outcome_state[,23])
    
    if(outcome_name == "heart attack") {
      outbest <- outcome_state[order(outcome_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, na.last=TRUE),]
      
    }
    
    if(outcome_name == "heart failure") {
      outbest <- outcome_state[order(outcome_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, na.last=TRUE),]
    }
    
    if(outcome_name == "pneumonia") {
      outbest <- outcome_state[order(outcome_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, na.last=TRUE),]
    }
    
    outbest[1,2]
    
}

rankhospital <- function(state, outcome_name, num = "best") {
    # we'll use global outcome
    # outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    #
    
    if( outcome_name != "heart attack" && outcome_name != "heart failure" && outcome_name != "pneumonia") {
      stop("invalid outcome")
    }
    
    if (nrow(outcome[outcome$State == state,]) == 0) {
      stop("invalid state")
    }
    
    
    outcome_state <- outcome[outcome$State == state,]
    outcome_state[,11] <- as.numeric(outcome_state[,11])
    outcome_state[,17] <- as.numeric(outcome_state[,17])
    outcome_state[,23] <- as.numeric(outcome_state[,23])
    
    if(outcome_name == "heart attack") {
      outbest <- outcome_state[order(outcome_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, outcome_state$Hospital.Name, na.last=TRUE),]
      outbest <- outbest[ complete.cases(outbest[, c(11)]), ]
    }
    
    
    if(outcome_name == "heart failure") {
      outbest <- outcome_state[order(outcome_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, outcome_state$Hospital.Name, na.last=TRUE),]
      outbest <- outbest[ complete.cases(outbest[, c(17)]), ]
    }
    
    if(outcome_name == "pneumonia") {
      outbest <- outcome_state[order(outcome_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, outcome_state$Hospital.Name, na.last=TRUE),]
      outbest <- outbest[ complete.cases(outbest[, c(23)]), ]
    }
    
    if (num == "best") {
      outbest[1, 2]
      
    } else if (num == "worst") {
      outbest[nrow(outbest), 2]
    } else if (num <= nrow(outbest)) {
      outbest[num, 2]
    } else {
      NA
    }
  }

rankall <- function(outcome_name, num = "best") {
  # we'll use global outcome
  # outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  #
  result <- data.frame( hospital=character(), state=character())
  for( s in unique(outcome$State) ) {
    result <- rbind(result, data.frame(hospital = rankhospital(s, outcome_name, num), state=s))
  }
  result
}