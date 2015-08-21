
pollutantmean <- function (directory, pollutant, id = 1:332) {

  ## pollutantmean <- function() { ##directory, pollutant, id = 1:332
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
    
  pollutant_data <- data.frame()
  pollutant_file <- data.frame()
  
  for (n in id) 
    {
  if (n < 10)  { xx<- paste0("00", n)}
  else if (n>=10 && n<100) {xx<- paste0("0", n)}
  else {xx<- paste0(n)}  
    
  file_read <- paste0(directory,"/",xx,".csv")

 
  pollutant_file <- read.csv(file= file_read , header=TRUE, sep = ",")  
  pollutant_data <- rbind.data.frame(pollutant_data, pollutant_file, deparse.level = 1)  
  }
 
  if (pollutant == "sulfate") {outcome <- mean(pollutant_data[, 2], na.rm = TRUE)}
  else if (pollutant == "nitrate") {outcome <- mean(pollutant_data[, 3], na.rm = TRUE)}
 
  
  print(outcome)
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

  pollutant_data <- data.frame()
  pollutant_file <- data.frame()
  observation_quality <- data.frame()
  row_num <- 1
  
  for (n in id) 
  {
    if (n < 10)  { xx<- paste0("00", n)}
    else if (n>=10 && n<100) {xx<- paste0("0", n)}
    else {xx<- paste0(n)}  
    
    file_read <- paste0(directory,"/",xx,".csv")
    
    
    pollutant_file <- read.csv(file= file_read , header=TRUE, sep = ",")  
    good_cases <- complete.cases(pollutant_file)
    pollutant_data <- pollutant_file[good_cases, ]
    observation_quality[row_num,1]<- pollutant_data[1,4]
    observation_quality[row_num,2]<- nrow(pollutant_data)
    
    row_num <- row_num + 1
    
  }
  
  colnames(observation_quality) <- c("id", "nobs")
  ##good_vector <- complete.cases(observation_quality)
  ##observation_quality <- observation_quality[good_vector, ]
  
  print(observation_quality)  
  
  
  
}

  

