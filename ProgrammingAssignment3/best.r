best <- function(state, outcome=NULL) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
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
  minimail<-min(as.numeric(as.matrix(statehospitalnameoutcome[outcome])))  
  besthospital<-statehospitalnameoutcome[as.numeric(as.matrix(statehospitalnameoutcome[outcome])) == minimail, "Hospital.Name"]  
  as.character(besthospital[[1]])

}