## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'threshold' is a numeric vector of length 1 indicating the
## number of completely observed observations (on all
## variables) required to compute the correlation between
## nitrate and sulfate; the default is 0

## Return a numeric vector of correlations

corr <- function(directory, threshold = 0) {
  id = list.files(directory);
  #print(id);
  v_variances <- c();
  for(i in seq_along(id)) {
    name_file <-paste0(directory,"/",id[i]);
    current_file <- read.csv(name_file);
    v_complete_cases_nitrate <- current_file[!is.na(current_file[,2])&!is.na(current_file[,3]), "nitrate"];
    v_complete_cases_sulfate <- current_file[!is.na(current_file[,2])&!is.na(current_file[,3]), "sulfate"];

    all_complete_cases_nitrate<-c();
    all_complete_cases_sulfate<-c();
    #print(v_complete_cases_nitrate);
  	
    for(j in seq_along(v_complete_cases_nitrate)) {
      if(j==1 & length(all_complete_cases_nitrate)==0){
        all_complete_cases_nitrate <- v_complete_cases_nitrate[j];
        all_complete_cases_sulfate <- v_complete_cases_sulfate[j];
      }else{
        all_complete_cases_nitrate <- rbind(all_complete_cases_nitrate,v_complete_cases_nitrate[j]);
        all_complete_cases_sulfate <- rbind(all_complete_cases_sulfate,v_complete_cases_sulfate[j]);          
      }  
    }      
   
    #& length(correlation)>0 & !is.na(correlation)
    if(length(all_complete_cases_nitrate)>threshold & length(all_complete_cases_sulfate)>threshold){
      correlation <- cor(all_complete_cases_nitrate, all_complete_cases_sulfate);
      #print(correlation);
      if(length(v_variances)==0 & length(correlation)>0 & !is.na(correlation)){
        v_variances <- correlation;
      }else if(length(correlation)>0 & !is.na(correlation)){
        v_variances <- rbind(v_variances,correlation);
        
      }
    }
  }
  return(v_variances);  
}