rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
#   validatestate<-function(state1){
#     if(state1==state) {return(1)}
#     else {return(0)};
#   }
#   
#   statescorrect<-sum(sapply(      Filter(Negate(is.null), as.character(file[["State"]])), validatestate))
#   if(statescorrect==0)
#     stop("invalid state")    
  
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
  statehospitalnameoutcome <- file[!is.na(file[outcome]) & file[outcome]!="Not Available", c("Hospital.Name", outcome, "State")]
  orderedstatehospitalnameoutcome <- statehospitalnameoutcome[order(statehospitalnameoutcome["State"], as.numeric(as.vector(statehospitalnameoutcome[[outcome]])),statehospitalnameoutcome["Hospital.Name"]), c("Hospital.Name", "State",outcome)]
  hospitals<-as.character(orderedstatehospitalnameoutcome[,"Hospital.Name"])
  states<-as.character(orderedstatehospitalnameoutcome[,"State"])
  #print(orderedstatehospitalnameoutcome)

  longitudesporestado<- tapply(orderedstatehospitalnameoutcome[[outcome]], orderedstatehospitalnameoutcome[["State"]], length)
  
  maxlength <- max(tapply(orderedstatehospitalnameoutcome[[outcome]], orderedstatehospitalnameoutcome[["State"]], length))
  vectorhospitals<-c()
  vectorstates<-c()
  vectorrankingbystate<-c()
  stateanterior<-0
  vectornamerows<-c()
  j<-1
  k<-1
  for(i in 1:length(states)){  
    if(length(stateanterior) > 0 & stateanterior==states[i]){
      j<-j+1
    }else if(j<maxlength & i > 1){
      while(j<maxlength){
        vectorhospitals <- rbind(vectorhospitals, NA)
        vectorstates <- rbind(vectorstates, stateanterior)
        vectorrankingbystate <- rbind(vectorrankingbystate,j) 
        vectornamerows <- rbind(vectornamerows,k*maxlength+j)        
        j<-j+1        
      }
      j<-1
      k<-k+1
    }
    else{
      j<-1
      k<-k+1
    }
    vectorhospitals <- rbind(vectorhospitals, hospitals[i])
    vectorstates <- rbind(vectorstates, states[i])
    vectorrankingbystate <- rbind(vectorrankingbystate,j) 
    vectornamerows <- rbind(vectornamerows,(k*maxlength)+j)
    stateanterior<-states[i]
  }
  hospital<-vectorhospitals
  state<-vectorstates
  orderedstatehospitalnameoutcome <- data.frame(hospital, state, vectorrankingbystate)  #, row.names=vectornamerows

  if(num=="best")
    rank <- orderedstatehospitalnameoutcome[orderedstatehospitalnameoutcome["vectorrankingbystate"]==1,c("hospital", "state")]
  else if(num=="worst")
    #rank <- orderedstatehospitalnameoutcome[orderedstatehospitalnameoutcome["vectorrankingbystate"]==length(orderedstatehospitalnameoutcome),c("hospital", "state")]  
    rank <- orderedstatehospitalnameoutcome[orderedstatehospitalnameoutcome["vectorrankingbystate"]==longitudesporestado[orderedstatehospitalnameoutcome[,"state"],1],c("hospital", "state")]  

  else if((length(vectorhospitals) < num))
    rank <- NA
  else
    rank <- orderedstatehospitalnameoutcome[orderedstatehospitalnameoutcome["vectorrankingbystate"]==num,c("hospital", "state")]
  #rank <- orderedstatehospitalnameoutcome[tapply(orderedstatehospitalnameoutcome[["Hospital.Name"]], orderedstatehospitalnameoutcome[["State"]], rank),]
  #rank <- tapply(orderedstatehospitalnameoutcome[[outcome]], orderedstatehospitalnameoutcome[["State"]], order)
  #rank <- tapply(orderedstatehospitalnameoutcome[[outcome]], orderedstatehospitalnameoutcome[["State"]], length)
  #rank <- orderedstatehospitalnameoutcome[["vectorrankingbystate"]]
  #rank
  rank
}