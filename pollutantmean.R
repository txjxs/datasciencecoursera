pollutantmean <- function(directory, pollutant, id = 1:332){
  #programe to find the mean of the given pollutant 
  as.character(directory)
  as.character(pollutant)
  poldata <- NULL
  for(i in id){
    if(i < 10){
      path1 <- paste0(getwd(),"/", directory,"/","00",i,".csv") 
      dat <- read.csv(path1)
      #rqdat <- dat$pollutant
      rqdat <- dat[[pollutant]]
      poldata <- c(poldata, rqdat)
      }
    if(i >= 10 & i<=99){
      path1 <- paste0(getwd(),"/", directory,"/","0",i,".csv") 
      dat <- read.csv(path1)
      #rqdat <- dat$pollutant
      rqdat <- dat[[pollutant]]
      poldata <- c(poldata, rqdat)
      }
    if(i>99){
      path1 <- paste0(getwd(),"/", directory,"/",i,".csv") 
      dat <- read.csv(path1)
      #rqdat <- dat$pollutant
      rqdat <- dat[[pollutant]]
      poldata <- c(poldata, rqdat)}
    }
  #print(class(poldata))
  mean(poldata, na.rm = TRUE)
}

#Write a function that reads a directory full of files and reports the number of
#completely observed cases in each data file. The function should return a data
#frame where the first column is the name of the file and the second column is 
#the number of complete cases. A prototype of this function follows
complete <- function(directory, id){
  df <- NULL
  nobs <- NULL
  for( i in id){
    if(i <10){
      path <- paste0(getwd(),"/",directory,"/","00",i,".csv")
      data <- read.csv(path)
      tnobs <- sum(complete.cases(data))
      nobs <- c(nobs, tnobs)
      df <- cbind(id,nobs)
    }
    if(i>=10 & i<=99){
      path <- paste0(getwd(),"/",directory,"/","0",i,".csv")
      data <- read.csv(path)
      tnobs <- sum(complete.cases(data))
      nobs <- c(nobs, tnobs)
      df <- cbind(id,nobs)
    }
    if(i>99){
      path <- paste0(getwd(),"/",directory,"/",i,".csv")
      data <- read.csv(path)
      tnobs <- sum(complete.cases(data))
      nobs <- c(nobs, tnobs)
      df <- cbind(id,nobs)
    }
  }
  return(df)
} 

corr <- function(directory, threshold = 0){
  cor_vec <- NULL
  for(i in 1:332){
    data <- read.csv(list.files(directory, full.names = TRUE)[i])
    sum <- sum(complete.cases(data))
    if(sum > threshold){
      cor_vec <- c(cor_vec,cor(data["sulfate"], data["nitrate"], use = "complete.obs"))
    }
  }
  return(cor_vec)
}