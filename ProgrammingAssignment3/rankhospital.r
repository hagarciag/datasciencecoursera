rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  validatestate<-function(state1){
    if(state1==state) {return(1)}
    else {return(0)};
  }
  
  statescorrect<-sum(sapply(      Filter(Negate(is.null), as.character(file[["State"]])), validatestate))
  if(statescorrect==0)
    stop("invalid state")    
  
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
    stop("invalid outcome") 
  }
  
  file<-read.csv(file = "outcome-of-care-measures.csv")
  statehospitalnameoutcome <- file[!is.na(file["State"]) & !is.na(file[outcome]) & file["State"]==state & file[outcome]!="Not Available", c("Hospital.Name", outcome)]
  orderedstatehospitalnameoutcome <- statehospitalnameoutcome[order(as.numeric(as.vector(statehospitalnameoutcome[[outcome]])),statehospitalnameoutcome["Hospital.Name"]), "Hospital.Name"]
  #print(orderedstatehospitalnameoutcome)
  if(num=="best")
    rank <- as.character(orderedstatehospitalnameoutcome[[1]])
  else if(num=="worst")
    rank <- as.character(orderedstatehospitalnameoutcome[[length(orderedstatehospitalnameoutcome)]])
  else if((length(orderedstatehospitalnameoutcome) < num))
    rank <- NA
  else
    rank <- as.character(orderedstatehospitalnameoutcome[[num]])
  rank

}