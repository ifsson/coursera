corr <- function(directory ="specdata", threshold = 0) {
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
  result_vector <- vector()
  row_num <- 1
  
  for (n in 1:332) 
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
    observation_quality[row_num,3]<- cor(pollutant_data[,2],pollutant_data[,3])
    
    row_num <- row_num + 1
    
  }
  
  colnames(observation_quality) <- c("id", "nobs", "correlation")

  

  ##print(pollutant_data)  
  
  
  
  
  observation_quality2 <- subset(observation_quality, nobs > threshold)
  
  result_vector <- observation_quality2[,3]
  ##print(result_vector)  
  
}
