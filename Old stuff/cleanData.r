set.seed(1234)

rm(list=ls()) # just clean everything from the previous run

#install.packages(c("readxl", "plyr", "zoo", "forecast", "astsa","DEoptim","DEoptimR","parallel","foreach","doParallel","readr"))

library(readxl)
library(plyr)
library(zoo)
library(forecast)
library(astsa)
library(DEoptim)
library(DEoptimR)
library(parallel)
library(foreach)
library(doParallel)
library(readr)
library(xlsx)

dataFile <- paste(getwd(), "/", "monthlyData.xls", sep="")

addEuroGrowth <- function(frame) { 
  probs <- rep(TRUE, length(colnames(frame)))
  val <- c( probs, FALSE )
  id  <- c( seq_along(probs), 12.5 )
  
  frame[gsub("\\.\\.", "%", colnames(frame)[4])] <- NA
  
  frame <- frame[,colnames(frame)[order(id)]]
  
  return(frame)
}

# Format and clean the Reuters Excel Plugin
readCleanSheet <- function(file, sheetName) {
  
  # Get the number of columns to read with the right format
  countFrame <- read_excel(file, sheet = sheetName)
  nCol <- ncol(countFrame)
  
  # Read the frame with the right formats
  frame <- read_excel(file, sheet=sheetName, col_types = c("date", rep("numeric",nCol-1)) )
  
  # Convert to data frame format
  frame <- as.data.frame(frame)
  
  # Rename the first column as 'date'
  frame <- rename(frame, c("Code"="date")) 
  
  # Reformat the dates from quarters to calendar days
  frame$date <- as.Date(frame$date, format = "%Y%m%d")
  
  return(frame)
}

# Read the names of the sheets and store them as the names of the categories of variables (such as "GDP")
catN <- excel_sheets(dataFile)
catN <- catN[2:length(catN)]

# The list of countries
ctry <- c("AU", "CN", "EK", "JP", "NZ", "NW", "SD", "SW", "UK", "US")

# Read the sheet of the excel file with the FX series
df <- readCleanSheet(dataFile, "FX")

# Cut the day of the month from the dates
monthCut <- function(str) {
  return(substr(str, 1, 7))
}
df$date <- lapply(df$date, monthCut)

# Read the US data file and rename columns
# https://fred.stlouisfed.org/series/TWEXB
US <- read_excel(paste(getwd(), "/", "USDollar.xls", sep=""), "FRED Graph")
US <- as.data.frame(US)
US <- rename(US, c("observation_date"="date")) 
US <- rename(US, c("DTWEXM"="Index")) 

# Delete the first observation to start 
US <- US[-1,]

# Divide by 100 the index to obtain the same order of magnitude as the other series 
US$Index <- US$Index/100

## Add the US serie
df[1:length(US$Index),"US"] <- US$Index

# Create an index variable that indexes the observations starting at 1
df$index <- seq(1, nrow(df))

# Rename the columns with the shorthand list of countries in ctry
colnames(df) <- c("date", ctry, "index")

# Reorder the columns
df <- df[,c("index", "date", ctry)]

# Create a vector of column names for df
colN <- c("index", "date")

# #  To create the colN vector and create columns of NAs in df
# Loop over countries
for(i in 1:length(ctry)) { 
  # Add the FX return column 
  colN <- c(colN, ctry[i], paste(ctry[i], "r", sep=""))
  df[paste(ctry[i], "r", sep="")] <- NA
  
  #Loop over categories fpr every country
  for(j in 1:length(catN)) { 
    # Create the name of the level and growth variable for the particular country and category
    name <- paste(ctry[i], catN[j],sep="")
    gname <- paste(ctry[i], "g", catN[j],sep="")
   
    df[name] <- NA
    df[gname] <- NA
    colN <- c(colN, name, gname)
  }
}

# Reorder the columns in the colN order
df <- df[,colN]

# The number of categories is twice the lenght of the categories name vector
nCat <- length(catN)*2

# Store the starting date
startDate <- df$date[1]

##  Compute returns
# Loop over countries
for(i in 1:length(ctry)) {
  # Loop over time periods
  for (t in 2:nrow(df)) { 
    if(is.na(as.numeric(df[t,ctry[i]])) & is.na(as.numeric(df[(t-1),ctry[i]])) ) { 
      df[t, paste(ctry[i], "r", sep="")] <- NA
    } 
    else { 
      df[t, paste(ctry[i], "r", sep="")] <- ( as.numeric(df[t,ctry[i]]) / as.numeric(df[(t-1),ctry[i]]) - 1 )
    }
  }
}

# The euro variables for which we need to add the growth variable
euroAddG <- c("GDP", "ConSpending", "GovSpending", "GrossFixedCapInv", "Exports", "Imports", "UnempR", "GovDeficit", "CurrentAcc")

# Store the number of rows in df
dfSize <- nrow(df)

# To store a list of the variables that are found and those which are missing
found <- c()
missing <- c()

# # Populate the df data frame with data from the excel file sheets
# Loop over columns of df
for(i in 3:ncol(df)) { 
  # Get the column name
  coli <- colnames(df)[i]
  
  # Check that this is a column we need data for
  if(nchar(coli)>2 & substr(coli, 3, 3) != "r" ) { 
    
    # Extract the country
    country <- substr(coli, 1, 2)
    
    # Extract whether this is a growth variable and the category
    if(substr(coli, 3,3) == "g") { 
      growth <- TRUE
      category <- substr(coli, 4, nchar(coli))
      
    }
    else {
      growth <- FALSE
      category <- substr(coli, 3, nchar(coli))
    }
    
    # Load and clean data
    cData <- readCleanSheet(dataFile, category)

    # Add the growth column for Eurozone some frames 
    if(country == "EK" & category %in% euroAddG) { 
      cData <- addEuroGrowth(cData)   
    }
    
    cData <- subset(cData, date >= as.Date(startDate) )
  
    # Find the column names for that country
    ctryVec <- colnames(cData)[grepl(country, colnames(cData))]
    
    # Find the right one for growth or level
    if(growth) { 
      colFound <- ctryVec[grepl("%", ctryVec)]
    } else {
      colFound <- ctryVec[grepl("\\.\\.", ctryVec)]
    }
    
    # If the data is missing 
    if(length(colFound)==0) { 
      missing <- c(missing, coli)
      
      # If the data is found
    } else { 
      found <- c(found, coli)
      # If there are missing dates at the beginning
      if(nrow(cData[colFound]) < dfSize) { 
        missingCount <- dfSize - nrow(cData[colFound])
        
        # add NAs at the beginning of the data frame
        for(i in 1:missingCount) { 
          cData <- rbind(rep(NA, ncol(cData)), cData)
        }
      }
      df[coli] <- cData[colFound]
    }
    
    
    
    # Find the interest rate
    if(category == "IntRate") {
        IntCtry <- colnames(cData)[grepl(country, colnames(cData))]
        
        if(length(IntCtry)!=0 & growth == FALSE) {
          df[coli] <- cData[IntCtry]
        }
        
        if(growth) {
          for (t in 2:dfSize) { 
            if(is.na(as.numeric(df[t,levelIntRate])) & is.na(as.numeric(df[(t-1),levelIntRate])) ) { 
              df[t, paste(ctry[i], "r", sep="")] <- NA
            } 
            else { 
              df[t, coli] <- ( as.numeric(df[t,levelIntRate]) / as.numeric(df[(t-1),levelIntRate]) - 1 )
            }
            
            if(is.infinite(df[t, coli])) { 
                df[t, coli] <- 0
            }
          }
        } else { 
          levelIntRate <- coli  
        }
    }

  }
}

# Replace "NA" strings by the proper NA format function
makeTrueNA <- function(x) { 
  if(is.character(x) || is.factor(x)){
    is.na(x) <- x=="NA"; x
  } else {
    x
  }
}
df <- as.data.frame(lapply(df, makeTrueNA))

# Format the columns as numeric (drop factor levels)
for ( i in  3:ncol(df) ) {
  df[,i] <- as.numeric(as.character(df[,i]))
}


# Compute a backforecast for variables which have missing observations
# Loop over columns
for(i in 3:ncol(df)) { 
  # Get the column name
  coli <- colnames(df)[i]
  
  # Check that this is a column we need data for
  if(nchar(coli)>2 & substr(coli, 3, 3) != "r" ) { 
  
    tLow <- tHigh <- NA
    
    # Compute the earliest observation tLow
    for (t in 1:dfSize) {
      if(!is.na(df[t,coli]) & is.na(tLow)) { 
          tLow <- t 
      }
    }
    
    # Compute the latest observation tHigh
    if(!is.na(tLow) ) {
      if(is.na(df[dfSize,coli])) {
        for (t in dfSize:tLow) {
          if(is.na(df[t,coli])) { 
            tHigh <- t-1
          }
        }
      }
      else {
        tHigh <- dfSize
      }
    }
    
    # If we need to backforecast
    if(!is.na(tLow) & tLow >1) { 
      revTS <- rev(df[tLow:tHigh, coli])
      forecastL <- forecast(auto.arima(revTS,D=1), h = (tLow-1))
  
      for(Ft in ((tLow-1):1)) { 
        fPeriod <- tLow - Ft
        df[Ft, coli] <- forecastL[4]$mean[fPeriod]
      }
    }
    
    # If we need to forecast 
    if(!is.na(tHigh) & tHigh < dfSize) { 
      forecastH <- forecast(auto.arima(df[tLow:tHigh, coli],D=1), h = (dfSize-tHigh))
      for(Ft in ((tHigh+1):dfSize)) { 
        fPeriod <- Ft - tHigh
        df[Ft, coli] <- forecastH[4]$mean[fPeriod]
      }
    }
  
  }
}

 # Normalise cross sectionally
for(t in 1:dfSize) { 

  # The vector of countries in period t
  ctryT <- c()
  # Loop over the countries
  for ( c in seq(4, (ncol(df)-length(ctry)), (nCat+2) )  ) {
    # Only for currencies in period t
    if(!is.na(df[t,c]) & df[t,c] != 0) { 
      ctryT <- c(ctryT, colnames(df)[(c-1)])  
    }
  }

   # Vector of missing currencies
  toRemove <- ctry[! ctry %in% ctryT]

  for(i in catN) {
    # Get all the variables of the given category
    fullCat <- colnames(df)[grepl(i, colnames(df))]
    
    # Remove missing currency variables, which don't need to be standardised
    if( !identical(toRemove, character(0)) ) { 
      # Loop over the countries to remove
      for(r in toRemove){ 
        fullCat <- fullCat[!grepl(r, fullCat)]
      }
    }
    
    # Seperate the growth and levels into vectors
    level <- growth <- c()
    for (j in fullCat) { 
      if(substr(j, 3,3) == "g") { 
        growth <- c(growth, j)
      } else { 
        level <- c(level, j)
      }
    }
    
    #### For the levels
    # Collect the observations for the category
    vecXiLevel <- c()
    for(j in level) {
      if(!is.na(df[t,j])) { 
        vecXiLevel <- c( vecXiLevel, as.numeric(as.character(df[t,j])) )
      }
    }
    
    # If observations are found
    if(!is.null(vecXiLevel)) { 
      # Normalise the vector
      vecXiLevel <- (vecXiLevel - mean(vecXiLevel))/sd(vecXiLevel)
      
      I <- 1
      for(j in level) { 
        if(!is.na(df[t,j])) { 
          df[t,j] <- vecXiLevel[I]
          I <- I + 1
        }
      }    
    }

    ### For the growth
    # Collect the observations for the category
    vecXiGrowth <- c()
    for(j in growth) {
      if(!is.na(df[t,j])) { 
        vecXiGrowth <- c( vecXiGrowth, as.numeric(as.character(df[t,j])) )
      }
    }
    
    # If observations are found
    if(!is.null(vecXiGrowth)) { 
      # Normalise the vector
      vecXiGrowth <- (vecXiGrowth - mean(vecXiGrowth))/sd(vecXiGrowth)
      
      I <- 1
      for(j in growth) { 
        if(!is.na(df[t,j])) { 
          df[t,j] <- vecXiGrowth[I]
          I <- I + 1
        }
      }    
    }
      
  }
}

# Replace NAs by 0s
df[is.na(df)] <- 0


# Remove the last column "rNA"
df$rNA <- NULL

# Save to a file
getwd()
write.csv(file="df.csv", x=df, row.names=FALSE)


df
wDF
```


Construct weights dataset
```{r}
u <- function(x, th) {
  return( ((1+x)^(1-th))/(1-th) )
}

# The window size
winSize <- 30

# Create a new data frame with a date column
wDF <- data.frame(df$date)
colnames(wDF) <- "date"
wDF$index <- seq(1,nrow(wDF))
wDF <- wDF[,c("index","date")]


## Create a vector of characteristic names
# Loop over categories
catA <- c()
for(j in 1:length(catN)) {
  catA <- c(catA, paste(catN[j],sep=""), paste("g", catN[j],sep=""))
}

# Add columns for the theta vectors
wDF[,catA] <- 0

# Add the columns of weights
for(i in 1:length(ctry)) {
  wDF[paste("w", ctry[i], sep="")] <- 0
}

# Degine the risk aversion parameter
gamma <- 5


```



Backtest computation

```{r}
t0 <- rep(0, nCat)

cl <- makeCluster(detectCores())
registerDoParallel(cl, cores = detectCores())
   
meanUOptim <- function(par) {
  # Add a column of utilities
  dft$u <- 0
  
  # Loop over every period
  for(t in 1:nrow(dft)) {
    
    # Define the period-t return
    Rpt <- 0
    
    # The vector of countries in period t
    ctryT <- c()
    # Loop over the countries
    for ( c in seq(4, (ncol(dft)-length(ctry)), (nCat+2) )  ) {
      # Only for currencies in period t
      if(dft[t,c] != 0) { 
        ctryT <- c(ctryT, colnames(dft)[(c-1)])  
      }
    }
    
    #Loop over the countries
    for ( c in seq(4, (ncol(dft)-length(ctry)), (nCat+2) )  ) {
      if(colnames(dft)[(c-1)] %in% ctryT) { 
        #Get the weight of country c
        wC <- ( 1/length(ctryT) + ( (1/length(ctryT))*( t(as.matrix(par)) %*% as.numeric(dft[t,c:(c+nCat-1)]) ) ) )
        
        # Add to the the periot t return with multiplying by period t return
        Rpt <- Rpt + wC*dft[t,c]  
      }
    }
    
    dft$u[t] <- u(Rpt, gamma)
  }
  
  #print(-mean(dft$u))

  return(-mean(dft$u))
}

out = foreach(t = (winSize+1):nrow(df), .packages = c("DEoptimR"), .combine = rbind) %dopar% {
  try({
    dft <- df[(t-winSize):(t-1),]
    theta <- JDEoptim(rep(-1.5, nCat), rep(1.5, nCat), meanUOptim, maxiter=5)
    
    return(theta$par)
  })
}
stopCluster(cl)

# Write the thetas to wDF
for(t in (1:(nrow(df)-winSize)) ) { 
  wDF[(t+winSize),(3:(length(theta$par)+2))] <- out[t,]
}


# Compute PPP weights, returns and utilities
wDF$rPPP <- wDF$u <- 0
for(t in (winSize+1):nrow(df)) { 
  # The vector of countries in period t
  ctryT <- c()
  # Loop over the countries
  for ( c in seq(4, (ncol(df)-length(ctry)), (nCat+2) )  ) {
    # Only for currencies in period t
    if(df[t,c] != 0) { 
      ctryT <- c(ctryT, colnames(df)[(c-1)])  
    }
  }
  
  #Loop over the countries
  Rpt <- 0
  for ( c in seq(4, (ncol(df)-length(ctry)), (nCat+2) )  ) {
    if(colnames(df)[(c-1)] %in% ctryT) { 
      #Get the weight of country c
      wDF[t, paste("w",colnames(df)[(c-1)],sep="")] <- ( 1/length(ctryT) + ( (1/length(ctryT))* (t(as.matrix(as.numeric(wDF[t,3:(nCat+2)]))) %*% as.numeric(df[t,c:(c+nCat-1)]))) )

      # Add to the the periot t return with multiplying by period t return
      Rpt <- Rpt + as.numeric(wDF[t, paste("w",colnames(df)[(c-1)],sep="")])*df[t,c]     
    }
  }

  wDF$rPPP[t] <- Rpt
  wDF$u[t] <- u(Rpt, gamma)
}

# Compute the PPP cumulative return
wDF$PPP <- 1
for(t in (winSize+1):nrow(df)) { 
  wDF$PPP[t] <- (1 + wDF$rPPP[t])*wDF$PPP[(t-1)]
}


## The 1/N portfolio
# compute the 1/N return
wDF$rOneN <- 0
for(t in (winSize+1):nrow(df)) { 
  # The vector of countries in period t
  ctryT <- c()
  # Loop over the countries
  for ( c in seq(4, (ncol(df)-length(ctry)), (nCat+2) )  ) {
    # Only for currencies in period t
    if(df[t,c] != 0) { 
      ctryT <- c(ctryT, colnames(df)[(c-1)])  
    }
  }

  # The vector of returns
  returnT <- c()
  
  # Loop over the countries
  for ( c in seq(4, (ncol(df)-length(ctry)), (nCat+2) )  ) {
    if(colnames(df)[(c-1)] %in% ctryT) { 
      returnT <- c(returnT, df[t,colnames(df)[c]])
    }
  }

  wDF$rOneN[t] <- mean(returnT)
}

# Compute the 1/N cumulative return
wDF$OneN <- 1
for(t in (winSize+1):nrow(df)) { 
  wDF$OneN[t] <- (1 + wDF$rOneN[t])*wDF$OneN[(t-1)]
}


# To read a dataFrame
setwd("/Users/francois/Dropbox/Francois dissertation/results")
wDF <- as.data.frame(read_csv(file = "PPP.csv"))
wDF


#write.csv(file="wDF.csv", x=wDF, row.names=FALSE)


wDFOld <- wDF

plot(wDF$PPP[winSize:nrow(wDF)], type="l")

plot(wDF$rPPP[winSize:nrow(wDF)], type="l")


t <- 67
for ( c in seq(4, (ncol(df)-length(ctry)), (nCat+2) )  ) {
  print(c(colnames(df)[c-1],df[t,c]))
}

wDF

# Density plot of the theta elements
th <- c()
for(c in 3:53) { 
  for(t in 31:125) { 
    th <- c(th, wDF[t,c])
  }  
}
plot(density(th, adjust = 0.01))

# Density plot of the sum of theta vectors 
sumAbs <- c()
for(t in 31:125) { 
  sumAbs <- c(sumAbs, sum(abs(wDF[t,3:53])))
}
plot(density(sumAbs, adjust = 0.2))

# Density plot of the sum of theta vectors 
sumSq <- c()
for(t in 31:125) { 
  sumSq <- c(sumSq, sum((wDF[t,3:53])^2))
}
plot(density(sumSq, adjust = 0.3))

```




```{r}
# Plot of leverage
lev <- c()
for(t in (winSize+1):nrow(wDF)) { 
  print( c(t, sum(wDF[t,55:63])) )
  lev <- c(lev, sum(wDF[t,55:63]) )
}
plot(lev, type="l")

levOld <- c()
for(t in (winSize+1):nrow(wDFOld)) { 
  print( c(t, sum(wDFOld[t,55:63])) )
  levOld <- c(levOld, sum(wDFOld[t,55:63]) )
}
plot(levOld, type="l")


ctryV <- c()
for(t in (winSize+1):nrow(df)) { 
  # The vector of countries in period t
  ctryT <- c()
  # Loop over the countries
  for ( c in seq(4, (ncol(df)-length(ctry)), (nCat+2) )  ) {
    # Only for currencies in period t
    if(df[t,c] != 0) { 
      ctryT <- c(ctryT, colnames(df)[(c-1)])  
    }
  }
  
  ctryV <- c(ctryV, length(ctryT))
  #print(c(t, levOld[t-winSize], lev[t-winSize], ctryT))
  
}

plot(as.Date(wDF$date[(winSize+1):nrow(wDF)]),levOld,type="l", ylim=c(0.85, 1.15), col="black")
par(new=TRUE)
plot(as.Date(wDF$date[(winSize+1):nrow(wDF)]),lev,type="l", ylim=c(0.85, 1.15), col="red")
par(new=TRUE)
plot(ctryV/8, ylim=c(0.85, 1.15))

wDF

```












