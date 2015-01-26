best <- function(state, outcome=NULL) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  #State
  #Hospital.Name
  #invalid state
  if(outcome=="heart attack")
    outcome<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  else if(outcome=="heart failure")
    outcome<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  else if(outcome=="pneumonia")
    outcome<-"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  else{
    print("invalid outcome")
    stop 
  }
  
  file<-read.csv(file = "ProgrammingAssignment3/outcome-of-care-measures.csv")
  statehospitalnameoutcome <- file[file["State"]==state & file[outcome]!="Not Available", c("Hospital.Name", outcome)]
  #besthospital<-min(as.numeric(as.matrix(statehospitalnameoutcome[outcome])), na.rm = TRUE)
  minimail<-print(min(as.numeric(as.matrix(statehospitalnameoutcome[outcome]))))
  besthospital<-statehospitalnameoutcome[as.numeric(as.matrix(statehospitalnameoutcome[outcome])) == minimail, c("Hospital.Name", outcome)]  
  besthospital
}