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
  
  file<-read.csv(file = "outcome-of-care-measures.csv")
  statehospitalnameoutcome <- file[!is.na(file["State"]) & !is.na(file[outcome]) & file["State"]==state & file[outcome]!="Not Available", c("Hospital.Name", outcome)]
  minimail<-min(as.numeric(as.matrix(statehospitalnameoutcome[outcome])))
  besthospital<-statehospitalnameoutcome[as.numeric(as.matrix(statehospitalnameoutcome[outcome])) == minimail, "Hospital.Name"]  
  as.character(besthospital[[1]])
}