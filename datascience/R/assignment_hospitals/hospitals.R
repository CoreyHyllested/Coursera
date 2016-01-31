#hist(outcome.heart_attacks.numeric)

data.outcome = read.csv("outcome-of-care-measures.csv", colClasses = 'character')
data.updated = transform(data.outcome, 
                     Mortality.attack    = as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), 
                     Mortality.failure   = as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), 
                     Mortality.pneumonia = as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))

outcomes = data.updated[, c(1,2,7,47:49)]

outcomes.ha.clean = outcomes[!is.na(outcomes$Mortality.attack),    ]
outcomes.hf.clean = outcomes[!is.na(outcomes$Mortality.failure),   ]
outcomes.pn.clean = outcomes[!is.na(outcomes$Mortality.pneumonia), ]

outcomes.ha.sort = outcomes.ha.clean[order(outcomes.ha.clean$Mortality.attack),    ]
outcomes.hf.sort = outcomes.hf.clean[order(outcomes.hf.clean$Mortality.failure),   ]
outcomes.pn.sort = outcomes.pn.clean[order(outcomes.pn.clean$Mortality.pneumonia), ]




best.state <- function(state, outcome, sz = 1) {
  if (!state %in% state.abb) {    stop("invalid state")  }
  
  if (outcome == 'ha') {
    data = outcomes.ha.sort
  } else if (outcome == 'hf') {
    data = outcomes.hf.sort
  } else if (outcome == 'pn') {
    data = outcomes.pn.sort
  } else {
    stop('actual values are ha, hf, pn')
  }
  data.by_state  = split(data, data$State)
  data.for_state = data.by_state[state]
  
  rank = 1:nrow(data.for_state[[1]])
  data.for_state[[1]]$Rank <- rank
#  data$rank <- 1:nrow(data)
  head(data.for_state[[1]], sz)
}


best.all <- function(outcome, sz = 1) {
  if (outcome == 'ha') {
    data = outcomes.ha.sort[, c(2,3,4)]
  } else if (outcome == 'hf') {
    data = outcomes.hf.sort[, c(2,3,5)]
  } else if (outcome == 'pn') {
    data = outcomes.pn.sort[, c(2,3,6)]
  } else {
    stop('actual values are ha, hf, pn')
  }
  #data = data[, c(3,2)] #  #data = data[, c(data$State, data$Hospital.Name)]
  
  data.by_state  = split(data, data$State)
  #rank = 1:nrow(data.for_state[[1]])
  #data.for_state[[1]]$Rank <- rank
  
  lapply(data.by_state, function (st) { head(st, sz)})
}
