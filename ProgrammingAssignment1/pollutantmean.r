## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'pollutant' is a character vector of length 1 indicating
## the name of the pollutant for which we will calculate the
## mean; either "sulfate" or "nitrate".

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return the mean of the pollutant across all monitors list
## in the 'id' vector (ignoring NA values)


pollutantmean <- function(directory, pollutant, id = 1:332) {
		for(i in seq_along(id)) {
			name_with_left_pad <- sprintf("%03d", id[i]);
			name_file <-paste0(directory,"/",name_with_left_pad,".csv");
			current_file <- read.csv(name_file);
			if(i==1){
				vector_polutant_entries <- current_file[pollutant];			
			}
			else{
				vector_polutant_entries <- rbind(vector_polutant_entries, current_file[pollutant]);			
			}
		}
		mean_value <- round(mean(vector_polutant_entries[!is.na(vector_polutant_entries)]), digits = 3);
		return(mean_value);
}
