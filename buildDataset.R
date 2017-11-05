set.seed(1234)

rm(list=ls())

library(readxl)
library(plyr)
library(lubridate)
library(forecast)
library(mFilter)
library(TTR)

log100 <- function(serie) { 
  return(log(serie)*100)  
  #return(serie)
}


dataDir <- paste(getwd(), "/rawData/", sep="")

FX <- read_excel(paste(dataDir, "FEDFX.xlsx", sep=""), sheet = "Sheet1")

# Convert to data frame format
FX <- as.data.frame(FX)

# Delete the first observation to start  
FX <- FX[7:nrow(FX),]

ctries <- c("US", "AU", "EK", "NZ", "UK", "CN", "JP", "NW", "SD", "SW")

colnames(FX) <- c("Date", ctries)

FX$Date <- as.Date(FX$Date, format = "%Y-%m-%d")

# Convert to numeric 
for(i in 2:ncol(FX)) { 
  FX[,i] <- as.numeric(FX[,i])
}

inverse <- function(n) { 
  return(1/n) 
}

# To invert: CN, JP, NW, SD, SW (columns 7 to 11)
for(i in 7:11) { 
  FX[,i] <- unlist(lapply(FX[,i], inverse))
}

df <- data.frame(Date = seq(as.Date("1971/1/1"), as.Date("2017/09/24"), "months"))

df$Date <- format(as.Date(df$Date), "%Y-%m")

df[, ctries] <- NA

findRate <- function(date, country) {
  month <- substr(date,6,7)
  year <- substr(date,1,4)
  rateList <- subset(FX, format.Date(Date, "%m")==month & format.Date(Date, "%Y")==year)[country][,1]
  
  for(i in length(rateList):1) { 
    if(!is.na(rateList[i])) { 
      return(rateList[i])
    }
  }
  return(NA)
}


for(i in 1:nrow(df)) { 
  for(c in ctries) { 
    df[i,c] <- findRate(df[i,1], c)
  }
}

catN <- c("IPI", "M1", "CPI", "Int")
catF <- c("Inf", "YGap", "UIP", "PPP", "MF", "TR")

catA <- c(catN, catF)

# #  To create the colN vector and create columns of NAs in df
colN <- c("Date")
# Loop over countries
for(i in 1:length(ctries)) { 
  # Add the FX return, Forward Rate and Forward return columns 
  return <- paste(ctries[i], "r", sep="")
  Forward <- paste(ctries[i], "F", sep="")
  ForwardReturn <- paste(ctries[i], "Fr", sep="")
  colN <- c(colN, ctries[i], return, Forward, ForwardReturn)
  df[c(return, Forward, ForwardReturn)] <- NA
  
  #Loop over categories fpr every country
  for(j in 1:length(catA)) { 
    # Create the name of the particular country and category
    name <- paste(ctries[i], catA[j],sep="")
    df[name] <- NA
    colN <- c(colN, name)
  }
}

# Reorder the columns in the colN order
df <- df[,colN]

# Store the starting date
startDate <- df$Date[1]

##  Compute returns
# Loop over countries
for(i in 1:length(ctries)) {
  # Loop over time periods
  for (t in 2:nrow(df)) { 
    if(is.na(as.numeric(df[t,ctries[i]])) & is.na(as.numeric(df[(t-1),ctries[i]])) ) { 
      df[t, paste(ctries[i], "r", sep="")] <- NA
    } 
    else { 
      df[t, paste(ctries[i], "r", sep="")] <- ( as.numeric(df[t,ctries[i]]) / as.numeric(df[(t-1),ctries[i]]) - 1 )
    }
  }
}


### Add "IPI", "M1" and "CPI" variables
dataFile <- paste(dataDir, "monthlyData.xls", sep="")

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

#IPI <- readCleanSheet(dataFile, "IPI")

dfSize <- nrow(df)

# # Populate the df data frame with data from the excel file sheets

# Vector of variables to find in the excel sheet
sheetVars <- catN[catN != "Int"]

# Loop over columns of df
for(i in 3:ncol(df)) { 
  # Get the column name
  coli <- colnames(df)[i]
  
  # Check that this is a column we need data for
  if(substr(coli, 3, nchar(coli)) %in% sheetVars) { 
    country <- substr(coli, 1, 2)
    category <- substr(coli, 3, nchar(coli))
    
    # Load and clean data
    data <- readCleanSheet(dataFile, category)
    
    #Cut from the starting date of df
    data <- subset(data, date >= as.Date(paste(startDate,"-01",sep="")) )
    
    # Find the column names for that country
    colName <- colnames(data)[grepl(country, colnames(data))][1]
    
    if(!is.na(colName)) {
      # If there are missing dates at the beginning
      if(nrow(data[colName]) < dfSize && !is.na(colName)) { 
        missingCount <- dfSize - nrow(data[colName])
        
        # add NAs at the beginning of the data frame
        for(i in 1:missingCount) { 
          data <- rbind(rep(NA, ncol(data)), data)
        }
      }
      df[coli] <- log100(data[colName])
    }
  }
}

#### For Norway, use M2 instead of M1 
coli <- "NWM2"

country <- substr(coli, 1, 2)
category <- substr(coli, 3, nchar(coli))

# Load and clean data
data <- readCleanSheet(dataFile, category)

#Cut from the starting date of df
data <- subset(data, date >= as.Date(paste(startDate,"-01",sep="")) )

# Find the column names for that country
colName <- colnames(data)[grepl(country, colnames(data))][1]

# If there are missing dates at the beginning
if(nrow(data[colName]) < dfSize && !is.na(colName)) { 
  missingCount <- dfSize - nrow(data[colName])
  
  # add NAs at the beginning of the data frame
  for(i in 1:missingCount) { 
    data <- rbind(rep(NA, ncol(data)), data)
  }
}

# Plug in for Norway M1
df["NWM1"] <- log100(data[colName])


##### Add the New Zealand IPI
NZ <- read_excel(paste(dataDir,"NZIPI.xlsx", sep=""), sheet = "Feuil1")
NZ <- as.data.frame(NZ)

# Delete the first observation 
NZ <- NZ[6:nrow(NZ),]

# Delete 3rd and 4th columns
NZ[,3:4] <- NULL

# Rename columns
colnames(NZ) <- c("Date", "NZIPI")

# Convert to numeric 
for(i in 1:ncol(NZ)) { 
  NZ[,i] <- as.numeric(NZ[,i])
}

# Convert date format
NZ$Date <- as.Date(as.POSIXct(as.Date(NZ$Date,origin="1899-12-30")), format="%Y-%m-%d")

# Set all the days to 15
day(NZ$Date) <- 15

# Remove seasonality using a 4-period moving average filter
NZ$NZIPI = ma(NZ$NZIPI, order = 4, centre = T)

# Generate a sequence of monthly dates
DateSeq <- seq(NZ$Date[1],tail(NZ$Date,1),by="1 month")

# Create a new dataframe with the linear interpolation
NZMonthly <- data.frame(Date=DateSeq, Interp.Value=spline(NZ, method="natural", xout=DateSeq)$y)

# Merge the Interpolated values with the quarterly serie
NZInt <- merge(NZ, NZMonthly, by='Date', all.y = T)

# Format the dates to Year-Month
NZInt$Date <- format(as.Date(NZInt$Date), "%Y-%m")

# Rename the columns
colnames(NZInt) <- c("Date", "Quarterly", "Monthly")

# Write montly serie to df
for(i in 1:nrow(NZInt)) { 
  #print(NZInt[i,3])  
  df$NZIPI[df$Date==NZInt[i,1]] <- log100(NZInt[i,3])
}

#plot(test$Interp.Value)

#plot(as.ts(NZ$NZIPI))
#lines(trendNZ)


### Get interest rates
IR <- read_excel(paste(dataDir,"EurodepositRates.xlsx", sep=""), sheet = "Sheet2")
# Convert to data frame format
IR <- as.data.frame(IR)
# Delete the first observation to start 
IR <- IR[6:nrow(IR),]

colnames(IR) <- c("Date", "AU", "CN", "EK", "JP", "NZ", "NW", "SD", "SW", "US", "UK")

# Convert to numeric 
for(i in 1:ncol(IR)) { 
  IR[,i] <- as.numeric(IR[,i])
}

# Convert date format
IR$Date <- as.Date(as.POSIXct(as.Date(IR$Date,origin="1899-12-30")), format="%Y-%m-%d")


findIntRate <- function(date, country) {
  month <- substr(date,6,7)
  year <- substr(date,1,4)
  rateList <- subset(IR, format.Date(Date, "%m")==month & format.Date(Date, "%Y")==year)[country][,1]
  
  for(i in length(rateList):1) { 
    if(!is.na(rateList[i])) { 
      return(rateList[i])
    }
  }
  return(NA)
}

# Loop over columns of df
for(c in 3:ncol(df)) { 
  # Get the column name
  coli <- colnames(df)[c]
  
  # Check that this is a column we need data for
  if(substr(coli, 3, nchar(coli)) == "Int") { 
    
    country <- substr(coli, 1, 2)
    
    # Loop over rows for that column
    # Start at the 49th row of df, which corresponds to the first
    # date for which we have interest rate data in IR
    for(i in 49:nrow(df)) {
      df[i,c] <- findIntRate(df[i,1], country)
    }
    
  }
}


# Compute the forward exchange rate 
# Covered Interest parity: Ft = St*[(1+it)/(1+it*)]
# Loop over columns of df starting at 10 (skip the US)
for(c in 10:ncol(df)) { 
  # Get the column name
  coli <- colnames(df)[c]
  
  # Check that this is a column we need data for
  if(substr(coli, 3, nchar(coli)) == "F") { 
    
    country <- substr(coli, 1, 2)
    df[coli] <- (df[country]*((1+df[paste(country,"Int", sep="")]))/(1+df$USInt))
  }
}

##  Compute returns
# Loop over countries
for(i in 1:length(ctries)) {
  # Loop over time periods
  for (t in 2:nrow(df)) { 
    if(is.na(as.numeric(df[t, paste(ctries[i], "F", sep="")])) & is.na(as.numeric(df[(t-1), paste(ctries[i], "F", sep="")])) ) { 
      df[t, paste(ctries[i], "Fr", sep="")] <- NA
    } 
    else { 
      df[t, paste(ctries[i], "Fr", sep="")] <- ( as.numeric(df[t, paste(ctries[i], "F", sep="")]) / as.numeric(df[(t-1), paste(ctries[i], "F", sep="")]) - 1 )
    }
  }
}


## Compute the Inflation rate "Inf" 
# Loop over countries
for(i in 1:length(ctries)) {
  # Loop over time periods
  for (t in 13:nrow(df)) { 
    if(is.na(as.numeric(df[t, paste(ctries[i], "CPI", sep="")])) && is.na(as.numeric(df[(t-12), paste(ctries[i], "CPI", sep="")])) ) { 
      df[t, paste(ctries[i], "Inf", sep="")] <- NA
    } 
    else { 
      df[t, paste(ctries[i], "Inf", sep="")] <- (( as.numeric(df[t, paste(ctries[i], "CPI", sep="")]) / as.numeric(df[(t-12), paste(ctries[i], "CPI", sep="")]) - 1 ))*120
    }
  }
}

## Compute the Output Gap "YGap" using the Hodrik Prescott Filter
for(c in ctries) {
  for (t in 1:nrow(df)) {
    if(!is.na(df[t, paste(c, "IPI", sep="")])) {
      if(length(df[1:(t-1), paste(c, "IPI", sep="")][!is.na(df[1:(t-1), paste(c, "IPI", sep="")])]) > 5) {
        df[t, paste(c, "YGap", sep="")] <- tail(hpfilter(df[1:(t-1), paste(c, "IPI", sep="")][!is.na(df[1:(t-1), paste(c, "IPI", sep="")])], type="frequency", freq=14400)$cycle, n=1)
      }
    }
  }
}



## Compute macro factors

# Uncovered Interest Parity: 
for(c in 14:ncol(df)) { 
  # Get the column name
  coli <- colnames(df)[c]
  # Check that this is a column we need data for
  if(substr(coli, 3, nchar(coli)) == "UIP") { 
    country <- substr(coli, 1, 2)
    
    # For Sweden, take out 
    if(country == "SW") { 
        SWF <- df[paste(country,"F", sep="")]
        SWF <- SWF$SWF
        SWF[SWF <= 0] <- 0.05
        SWF <- log(SWF)
        df[coli] <- SWF - log(df[country])
    }
    else { 
      df[coli] <- log(df[paste(country,"F", sep="")]) - log(df[country])
    }
    
  }
}

# Purchasing Power Parity 
for(c in 14:ncol(df)) { 
  # Get the column name
  coli <- colnames(df)[c]
  # Check that this is a column we need data for
  if(substr(coli, 3, nchar(coli)) == "PPP") { 
    country <- substr(coli, 1, 2)
    
    df[coli] <- df[paste(country,"CPI", sep="")] - df["USCPI"] - log(df[country])
  }
}


# Monetary fundamentals 
for(c in 14:ncol(df)) { 
  # Get the column name
  coli <- colnames(df)[c]
  # Check that this is a column we need data for
  if(substr(coli, 3, nchar(coli)) == "MF") { 
    country <- substr(coli, 1, 2)
    
    df[coli] <- (df[paste(country,"M1", sep="")] - df["USM1"]) - (df[paste(country,"IPI", sep="")] - df["USIPI"]) - log(df[country])
  }
}



# Taylor Rule
for(c in 14:ncol(df)) { 
  # Get the column name
  coli <- colnames(df)[c]
  # Check that this is a column we need data for
  if(substr(coli, 3, nchar(coli)) == "TR") { 
    country <- substr(coli, 1, 2)
    
    df[coli] <- 1.5*(df[paste(country,"Inf", sep="")] - df["USInf"]) + 0.1*(df[paste(country,"YGap", sep="")]- df["USYGap"]) + 0.1*( log(df[country]) + df["USCPI"] - df[paste(country,"CPI", sep="")] )
  }
}


# Cut observations prior to 1988
df <- subset(df, as.numeric(substr(df$Date,1,4))>=1988)


# Add VIX data 
VOL <- as.data.frame(read_excel(paste(dataDir,"VIX.xlsx", sep=""), sheet = "VIX"))

ctryData <- c("AU", "NZ", "UK", "CN", "JP", "NW", "SD", "SW", "EK")

for(c in ctryData) { 
  df[paste(c, "VOL", sep="")] <- VOL[c]
}

#### TECHNICAL INDICATORS #### 

# dual MA rule 
S <- c(2, 3)
L <- c(9)

for(c in ctryData) { 
  for(s in S) { 
    for(l in L) { 
      # print(c(s, l))    
      macd <- as.data.frame(MACD(df[c], s, l, 1, maType="SMA" ))
      
      df[paste(c, "MA", s, "-", l, sep="")] <- macd$signal

    }
  }
}

# Bollinger Bands

B <- c(20, 50)

for(c in ctryData) { 
  for(b in B) { 
    band <- as.data.frame(BBands(df[c], n = b, "SMA", 2))
    df[paste(c, "BB", b, sep="")] <- band$pctB
  }
}

# Triple Smoothed Exponential Oscillator

for(c in ctryData) { 
  T <- as.data.frame(TRIX(df[c]))
  df[paste(c, "TSE", sep="")] <- (T$TRIX-T$signal)
}

# Donchian Channel

for(c in ctryData) { 
  donch <- as.data.frame(DonchianChannel(df[c]))
  df[paste(c, "DC", sep="")] <- (df[c] - donch$mid)
}

#### #### #### #### #### #### 



# Write raw dataset to disk
write.csv(file="raw.csv", x=df, row.names=FALSE)

#### Construct the data dataset

# Generate the list of categories
catData <- c("UIP", "PPP", "MF", "TR", "VOL")

for(s in S) { 
  for(l in L) {
    catData <- c(catData, paste("MA", s, "-", l, sep=""))
  }
}

for(b in B) { 
  catData <- c(catData, paste("BB", b, sep=""))
}

catData <- c(catData, "TSE", "DC")


nCountry <- length(ctryData)
colSelect <- c("r", catData)
colData <- c("Date")

for(c in colnames(df)) { 
  if(substr(c, 1, 2) %in% ctryData && substr(c, 3, nchar(c)) %in% colSelect) {
    colData <- c(colData, c)
  }  
}
data <- as.data.frame(df[colData])

####### Cross Sectional Normalisaton ####### 
for(t in 1:nrow(data)) { 
  #### The vector of countries in period t
  ctryT <- c()
  # Loop over the countries
  for ( ctry in ctryData ) {
    # Only for currencies in period t
    if(!is.na(data[t,paste(ctry, "r", sep="")])) { 
      ctryT <- c(ctryT, ctry)  
    }
  }
  ##### Vector of missing currencies
  toRemove <- ctryData[! ctryData %in% ctryT]
  
  #print(c(t, toRemove))
  
  for(cat in catData) {
    # Get all the variables of the given category
    cCat <- colnames(data)[grepl(cat, colnames(data))]
    
    # Remove missing currency variables toRemove, which don't need to be standardised
    # if toRemvoe is not empty:
    if( !identical(toRemove, character(0)) ) { 
      # Loop over the countries to remove
      for(r in toRemove){ 
        cCat <- cCat[!grepl(r, cCat)]
      }
    }
    #print(cCat)
    
    # Collect the observations for the category
    Xi <- c()
    for(j in cCat) {
      if(!is.na(data[t,j])) { 
        Xi <- c( Xi, as.numeric(as.character(data[t,j])) )
      }
    }
    # If observations are found
    if(!is.null(Xi)) { 
      # Normalise the vector
      Xi <- (Xi - mean(Xi))/sd(Xi)
      
      I <- 1
      for(j in cCat) { 
        if(!is.na(df[t,j])) { 
          data[t,j] <- Xi[I]
          I <- I + 1
        }
      }    
    }
    
  }
  
}

# Replace NAs by 0s when there is no data for monetary series
for(ctry in ctryData) { 
  for(cat in catData) { 
    data[paste(ctry, cat, sep="")][ is.na(data[paste(ctry, cat, sep="")]) ] <- 0
  }  
}

# Create a variable nCtry for the number of countries
data$nCtry = 0

for(t in 1:nrow(data)) { 
  #### The vector of countries in period t
  ctryT <- c()
  # Loop over the countries
  for ( ctry in ctryData ) {
    # Only for currencies in period t
    if(!is.na(data[t,paste(ctry, "r", sep="")])) { 
      ctryT <- c(ctryT, ctry)  
    }
  }
  data[t, "nCtry"] = length(ctryT)
}

# Replace NAs with 0s for Eurozone returns 
data$EKr[is.na(data$EKr)] <- 0

# Put 0s for EK variables prior to 1999-02
for(c in 1:ncol(data)) { 
  # Get the column name
  coli <- colnames(data)[c]
  country <- substr(coli, 1, 2)

  # Check that this is a column we need data for
  if(substr(coli, 3, nchar(coli)) %in% catData && country == "EK") { 
    data[,coli][as.Date(paste(data$Date,"-01",sep="")) < as.Date("1999-02-01")] <- 0
  }
}

# Reorder the columns to have returns in front
NewColNames <- c("Date", "nCtry")
for(ctry in ctryData) { 
  NewColNames <- c(NewColNames, paste(ctry, "r", sep=""))
}
for(ctry in ctryData) { 
  for(cat in catData) { 
    NewColNames <- c(NewColNames, paste(ctry, cat, sep=""))
  }  
}
data <- data[,NewColNames]

# Construct 1/Nt vector of length n for nCountry
vecNt <- function(n) { 
  Nt <- rep(0, nCountry)
  Nt[1:n] <- rep(1/n, n)
  return(Nt)
}

# The 1/N portfolio 
data$rOneN <- 0
for(t in 1:nrow(df)) { 
  data$rOneN[t] <- t(matrix(as.vector(as.numeric(vecNt(data[t,2]))))) %*% matrix(as.vector(as.numeric(data[t, 3:(2+nCountry)])))
}

# Compute the 1/N cumulative return
data$OneN <- 1
for(t in 2:nrow(data)) { 
  data$OneN[t] <- (1 + data$rOneN[t])*data$OneN[(t-1)]
}

# Add columns for the theta vectors
data[,catData] <- 0

# Add the columns of weights
for(i in 1:length(ctryData)) {
  data[paste("w", ctryData[i], sep="")] <- 0
}

# Write raw dataset to disk
write.csv(file="data.csv", x=data, row.names=FALSE)



