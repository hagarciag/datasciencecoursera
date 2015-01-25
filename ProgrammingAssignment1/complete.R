## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return a data frame of the form:
## id nobs
## 1  117
## 2  1041
## ...
## where 'id' is the monitor ID number and 'nobs' is the
## number of complete cases

complete <- function(directory, id = 1:332) {
  for(i in seq_along(id)) {
    name_with_left_pad <- sprintf("%03d", id[i]);
    name_file <-paste0(directory,"/",name_with_left_pad,".csv");
    current_file <- read.csv(name_file);
    v_complete_cases <- current_file[!is.na(current_file["sulfate"])&!is.na(current_file["nitrate"]), "nitrate"];
    nrows_complete_cases <- length(v_complete_cases);
    if(i==1){
      v_name_files <- id[i];			
      v_nrows <- nrows_complete_cases;
      v_colnames <- i;
    }
    else{
      v_name_files <- rbind(v_name_files, id[i]);			
      v_nrows <- rbind(v_nrows, nrows_complete_cases);  		
      v_colnames <- rbind(v_colnames,i);
    }
  }
  #print(v_name_files);
  #return(v_complete_cases);
  #mean_value <- round(mean(vector_polutant_entries[!is.na(vector_polutant_entries)]), digits = 3);
  vector_polutant_entries <- data.frame(v_name_files, v_nrows, row.names = v_colnames);
  colnames(vector_polutant_entries) <- c("id", "nobs");
  rownames(vector_polutant_entries) <- v_colnames;
  return(vector_polutant_entries);
}