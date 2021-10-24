
#pollutantmean function
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

#complete function
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
    pollutant_data <- content[na_fil_sulfate, ]
    #filtering out NAs in the columns under the nitrate pollutant
    na_fil_nitrate <- (!is.na(pollutant_data$nitrate))
    pollutant_data <- pollutant_data[na_fil_nitrate, ]
    nobs <- nrow(pollutant_data)
    reports <- rbind(reports, data.frame(id=file, nobs=nobs))
  }
  reports
}

corr<-function(directory, threshold=0){
  #initializing vector of correlations
  correlation<- c(numeric(0))
  
  complete_obv <- complete(directory)
  complete_obv <- complete_obv[complete_obv$nobs>=threshold, ]

  if(nrow(complete_obv)>0){
    for(file in complete_obv$id){
      #same path access as in the pollutant mean function. 
      path <- paste(getwd(), "/", directory, "/", sprintf("%03d", file), ".csv", sep = "")
      #reading content in the csv file
      content<-read.csv(path)
      #filtering out NAs in the columns under the sulfate pollutant
      na_fil_sulfate <- (!is.na(content$sulfate))
      pollutant_data <- content[na_fil_sulfate, ]
      #filtering out NAs in the columns under the nitrate pollutant
      na_fil_nitrate <- (!is.na(pollutant_data$nitrate))
      pollutant_data <- pollutant_data[na_fil_nitrate, ]
      
      sulfate <- pollutant_data["sulfate"]
      nitrate <- pollutant_data["nitrate"]
      
      correlation <- c(correlation, cor(sulfate, nitrate))
    }
  }
  correlation
}

cr <- corr("specdata", 150)
head(cr);summary(cr)






