pollutantmean <- function(directory, pollutant, id) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
		directory;
		#id;
		
		for(i in seq_along(id)) {
			#print(i)
			#name_with_left_pad <- str_pad(id[i], width=3, side="left", pad="0");
			name_with_left_pad <- sprintf("%03d", id[i]);
			name_file <-paste0(name_with_left_pad,".csv");
			x<-read.csv(name_file);
			print(x);
		}

        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
}

complete <- function(directory, id = 1:332) {
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
}


corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
}