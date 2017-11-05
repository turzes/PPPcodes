set.seed(1234)
rm(list=ls())

###### Model parameters ###### 

# Degine the CRRA risk aversion parameter 
gamma <- 5

# Estimation window size
winSize <- 90

# Number of iterations
iter = 500

# L1 norm regularisation parameter
L1 = 2

# L2 norm regularisaton parameter
L2 = 2

# Number of observations lag to use
lag <- 6

##############################

library(readr)
library(DEoptimR)
library(parallel)
library(foreach)
library(doParallel)
library(caret)

# CRRA Utility function
u <- function(x, th) {
  return( ((1+x)^(1-th))/(1-th) )
}

# Construct 1/Nt vector of length n for nCountry
vecNt <- function(n) { 
  Nt <- rep(0, nCountry)
  Nt[1:n] <- rep(1/n, n)
  return(Nt)
}

# Build a diagonal matrix
bdiag <- function (...) {
  if (nargs() == 1) 
    x <- as.list(...)
  else x <- list(...)
  n <- length(x)
  if (n == 0) 
    return(NULL)
  x <- lapply(x, function(y) if (length(y)) 
    as.matrix(y)
    else stop("Zero-length component in x"))
  d <- array(unlist(lapply(x, dim)), c(2, n))
  rr <- d[1, ]
  cc <- d[2, ]
  rsum <- sum(rr)
  csum <- sum(cc)
  out <- array(0, c(rsum, csum))
  ind <- array(0, c(4, n))
  rcum <- cumsum(rr)
  ccum <- cumsum(cc)
  ind[1, -1] <- rcum[-n]
  ind[2, ] <- rcum
  ind[3, -1] <- ccum[-n]
  ind[4, ] <- ccum
  imat <- array(1:(rsum * csum), c(rsum, csum))
  iuse <- apply(ind, 2, function(y, imat) imat[(y[1] + 1):y[2], 
                                               (y[3] + 1):y[4]], imat = imat)
  iuse <- as.vector(unlist(iuse))
  out[iuse] <- unlist(x)
  return(out)
}

# Read the dataset
data <- read_csv(paste(getwd(), "/data.csv", sep=""))
data <- as.data.frame(data)

# Count the number of categories nCat
catData <- c()
for(str in colnames(data)[grepl("AU", colnames(data))]) { 
  if(substr(str,3,3) == 'r') { 
  } else if(substr(str,1,1) == 'w') { 
  } else { 
    catData <- c(catData, substr(str, 3, nchar(str)))
  }
}
nCat <- length(catData)

# The number of countries
nCountry <- 9

# The function to maximise
meanUOptim <- function(par) {
  
  # Vectorisation of the for loop
  out <- -mean( u( ( (diagNt + ((par %*% matrix( as.vector(t(dft[,(3+nCountry):(2+nCountry+nCat*nCountry)])), nrow=nCat) ) %*% matNt)) %*% matR) , gamma) )

  return(out)
}

# The constraint function which is passed to the optimiser, with sum(abs(par)) - L1 = 0 for the L1 norm and sum(par^2) - L2 = 0 for the L2 norm
consEN <- function(par) { 
  c(sum(abs(par)) - L1, sum(par^2) - L2)  
}

# Define the space over which the optimiser searches
if(L1 <1 || L2<1) {
  searchSpace = max(L1, L2^2)
} else { 
  searchSpace = min(L1, L2^2)
}

# Initialise the parallel cluster 
cores=detectCores()
cl <- makeCluster(cores[1]) 
registerDoParallel(cl)

# Main parallel for-loop over time periods between winSize+lag and nrow(data)
out = foreach(t = (winSize+lag):nrow(data), .export=ls(envir=globalenv()), .packages = c("DEoptimR"), .combine = rbind) %dopar% {
  try({
    # Define a data frame with the estimation window data
    dft <- as.data.frame(data[(t-winSize-lag+1):(t-lag),])
    
    # Set seet for the stochastic differential evolution algorithm (for reproducable results)
    set.seed(1234)
    
    # Table of Nt values
    tab <- table(dft[,2])
    
    # Diagonal vector
    diagNt <- c()
    
    # loop over the table and add to diagNt using vecNt to build vectors
    I <- 1
    for(i in names(tab)) { 
      i <- as.numeric(i)
      diagNt <- c(diagNt, rep(vecNt(i), as.numeric(tab[I]) ) )
      I <- I + 1
    }
    
    # Construct a diagonal matrix with diagNt on the diagonal
    matNt <- matrix(0, nrow=nrow(dft)*nCountry, ncol=nrow(dft)*nCountry)
    diag(matNt) <- diagNt
    
    # Build the return matrix 
    Rvec <- as.vector(t(dft[,3:(2+nCountry)]))
    splitVec <- split(Rvec, cut(seq_along(Rvec), nrow(dft), labels = FALSE))
    matR <- bdiag(splitVec)
    
    # Call the optimiser, setting the searchspace, optimised function, constraint and a maximum number of iterations
    theta <- JDEoptim(rep(-searchSpace, nCat), rep(searchSpace, nCat), fn=meanUOptim, constr=consEN, maxiter=iter)#, trace = TRUE, triter = 20)
    
    # Return the result of the optimisation
    return(theta$par)
  })
}
# Close the threads 
stopCluster(cl)

# Log the raw output 
write.csv(file="out.csv", x=out, row.names=FALSE)

# Write the thetas in out to data
for(t in 1:((nrow(data)-winSize-lag+1) )) { 
  data[(t+winSize+lag-1), match("UIP", colnames(data)):(match("UIP", colnames(data))+nCat-1)] <- as.numeric(out[t,])
}

# Compute PPP weights, returns and utilities
data$rPPP <- data$u <- 0

for(t in (winSize+lag):nrow(data)) { 
  # weights
  data[t, (5+nCountry+nCountry*nCat+nCat):(4+nCountry+nCountry*nCat+nCat+nCountry)] <- vecNt(data[t,2]) + (1/data[t,2])* t(matrix(as.vector(as.numeric(data[t, match("UIP", colnames(data)):(match("UIP", colnames(data))+nCat-1)])))) %*% matrix(as.numeric(data[t,(3+nCountry):(2+nCountry+nCat*nCountry)]), nrow=nCat)
  
  # returns
  data$rPPP[t] <- t(matrix(as.numeric(data[t, (5+nCountry+nCountry*nCat+nCat):(4+nCountry+nCountry*nCat+nCat+nCountry)]))) %*% matrix(as.vector(as.numeric(data[t, 3:(2+nCountry)])))
  
  # utility
  data$u[t] <- u( data$rPPP[t], gamma)
}

# Compute the ParaPP cumulative return
data$ParaPP <- 1
for(t in (winSize+lag):nrow(data)) { 
  data$ParaPP[t] <- (1 + data$rPPP[t])*data$ParaPP[(t-1)]
}

# Diagnostics 
data$L1 <- 0
data$L2 <- 0

for(t in (winSize+lag):nrow(data)) { 
  data$L1[t] <- sum(abs(data[t, match("UIP", colnames(data)):(match("UIP", colnames(data))+nCat-1)]))
  data$L2[t] <- sum(data[t, match("UIP", colnames(data)):(match("UIP", colnames(data))+nCat-1)]^2)
}

# Sharp ratio
meanR <- mean(data$rPPP[(winSize+1):nrow(data)])*4
sdR <- sd(data$rPPP[(winSize+1):nrow(data)])*4
SR <- meanR / sdR

# Write to disk
write.csv(file=paste("PPP ", "L1=", L1, " L2=", L2, " iter=", iter, " lag=", lag, " SR= ", round(SR, digits = 3), " .csv", sep=""), x=data, row.names=FALSE)







