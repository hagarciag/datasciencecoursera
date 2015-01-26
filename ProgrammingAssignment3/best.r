best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  #State
  #Hospital.Name
#   Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
#   Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
#   Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
  file<-read.csv(file = "ProgrammingAssignment3/outcome-of-care-measures.csv")
  statehospitalnameoutcome <- file[file["State"]==state, c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
  statehospitalnameoutcome
}