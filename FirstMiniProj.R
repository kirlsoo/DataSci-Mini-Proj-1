#Kristen Ann Joy F. Luciano
#CMSC 197 - Intro to Data Sci 
#First Mini Project

#1 #pollutantmean function
pollutantmean <- function(directory, pollutant, id=1:332){
  #create a vector that gathers the data to calculate the mean
  mean_data<-c()
  #for loop to go through each file in the directory
  for (file in id){
    #path to access the files given each id. first get the current directory
    path <- paste(getwd(), 
                  "/", 
                  #add the directory argument passed in the function
                  directory, 
                  "/", 
                  #wrapper function to format id to 3 digit number as the file names are (i.e. 1 -> 001)
                  sprintf("%03d", file), 
                  #add csv extension
                  ".csv", 
                  sep = "")
    #access each file with the read.csv function
    content<-read.csv(path)
    #get data on the column of the chosen pollutant
    pollutant_data <- content[pollutant]
    #filtering out NAs
    na_filtered <- pollutant_data[!is.na(pollutant_data),]
    #adding the remaining data to the mean_data vector for calculation later.
    mean_data <- c(mean_data, na_filtered)
  }
  #calculate the mean of the objects in the vector
  mean(mean_data)
}

#2 #complete function
complete <- function(directory,id=1:332){
  #creating data frame named reports to store data to. columns are named id and nobs both
  #to hold numeric objects
  reports <-data.frame(id=numeric(0) , nobs=numeric(0))
  #for loop to go through each file
  for (file in id){
    #same path access as in the pollutant mean function. 
    path <- paste(getwd(), "/", directory, "/", sprintf("%03d", file), ".csv", sep = "")
    #reading content in the csv file
    content<-read.csv(path)
    #filtering out NAs in the columns under the sulfate pollutant
    na_fil_sulfate <- (!is.na(content$sulfate))
    #storing data with sulfate that are not NA to pollutant_data vector
    pollutant_data <- content[na_fil_sulfate, ]
    #filtering out NAs in the columns under the nitrate pollutant 
    #that are present in pollutant_data vector
    na_fil_nitrate <- (!is.na(pollutant_data$nitrate))
    #adding nitrate to the pollutant_mean vector
    #important note: the , inside the bracket is important to 
    #avoid undefined columns selected
    pollutant_data <- pollutant_data[na_fil_nitrate, ]
    #store the number of rows in nobs
    nobs <- nrow(pollutant_data)
    
    #combining the combined objects by rows in the data frame with file number under id
    #and combined number of rows under nobs
    reports <- rbind(reports, data.frame(id=file, nobs=nobs))
  }
  #return report result data frame
  reports
}

#3 #correlation function
corr<-function(directory, threshold=0){
  #initializing vector of correlations
  correlation<- c(numeric(0))
  #using the complete function to get the data frame of complete data
  complete_obv <- complete(directory)
  #filter and only get the rows with nobs greater than or equal to the threshold
  complete_obv <- complete_obv[complete_obv$nobs>=threshold, ]

  #accessing only if the number of rows in the data frame is greater than 0
  if(nrow(complete_obv)>0){
    #for each file in the id column in complete_obv
    for(file in complete_obv$id){
      #same path access as in the pollutant mean function. 
      path <- paste(getwd(), "/", directory, "/", sprintf("%03d", file), ".csv", sep = "")
      #reading content in the csv file
      content<-read.csv(path)
      #filtering out NAs in the columns under the sulfate pollutant
      na_fil_sulfate <- (!is.na(content$sulfate))
      #filtering out NAs in the columns under the nitrate pollutant 
      #that are present in pollutant_data vector
      pollutant_data <- content[na_fil_sulfate, ]
      #filtering out NAs in the columns under the nitrate pollutant
      na_fil_nitrate <- (!is.na(pollutant_data$nitrate))
      #adding nitrate to the pollutant_mean vector
      pollutant_data <- pollutant_data[na_fil_nitrate, ]
      
      #all NAs are filtered out and all are complete observations
      #storing all sulfate in sulfate 
      sulfate <- pollutant_data["sulfate"]
      #storing all sulfate in sulfate 
      nitrate <- pollutant_data["nitrate"]
      
      #getting the correlation with the cor() function and storing to 
      #the correlation vector
      correlation <- c(correlation, cor(sulfate, nitrate))
    }
  }
  #return the vector
  correlation
}

#4
#access the csv file and store to outcome
outcome <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
head(outcome)

#getting the 11th row data and make numeric type
outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11], 
     #main title
     main = "30-Day Death (Mortality) Rates for Heart Attack",
     #column color
     col="light blue",
     #x axis label
     xlab = "Deaths")






