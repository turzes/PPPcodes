set.seed(1234)
rm(list=ls())

###### Model parameters ###### 

# Degine the risk aversion parameter
gamma <- 5

# Number of iterations
iter = 500

# Range of L1 regularisation parameters to try
#L1Range <- c(0.5, 1, 2, 4)

# Range of L2 regularisation parameters to try
#L2Range <- c(0.5, 1, 2, 4)

# Range for both L1 and L2
LRange <- c(1, 2, 4, 8, 16)

# Number of folds and vCPU threads (for a quad core machine set k<8)
k <- 6

##############################

library(readr)
library(DEoptimR)
library(parallel)
library(foreach)
library(doParallel)
library(caret)

# CES Utility function
u <- function(x, th) {
  return( ((1+x)^(1-th))/(1-th) )
}

# Construct 1/Nt vector of length n for nCountry
vecNt <- function(n) { 
  Nt <- rep(0, nCountry)
  Nt[1:n] <- rep(1/n, n)
  return(Nt)
}

# To build a diagonal matrix
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
data <- read_csv("~/Dropbox/Francois2/data.csv")
data <- as.data.frame(data)

catData <- c()
for(str in colnames(data)[grepl("AU", colnames(data))]) { 
  if(substr(str,3,3) == 'r') { 
  } else if(substr(str,1,1) == 'w') { 
  } else { 
    catData <- c(catData, substr(str, 3, nchar(str)))
  }
}

nCat <- length(catData)
nCountry <- 9

# The function to maximise
meanUOptim <- function(par) {
  
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
  
  matNt <- matrix(0, nrow=nrow(dft)*nCountry, ncol=nrow(dft)*nCountry)
  diag(matNt) <- diagNt
  
  
  # Build the return matrix 
  Rvec <- as.vector(t(dft[,3:(2+nCountry)]))
  splitVec <- split(Rvec, cut(seq_along(Rvec), nrow(dft), labels = FALSE))
  matR <- bdiag(splitVec)
  
  # Vectorisation of the for loop
  out = -mean( u( ( (diagNt + ((par %*% matrix( as.vector(t(dft[,(3+nCountry):(2+nCountry+nCat*nCountry)])), nrow=nCat) ) %*% matNt)) %*% matR) , gamma) )

  return(out)
}


## The CrossValidation function 
crossVal <- function(L1, L2) { 

  # Read the dataset again
  data <- read_csv("~/Dropbox/Francois2/data.csv")
  data <- as.data.frame(data)
  
  #L1 = 5
  #L2 = 10
  
  consEN <- function(par) { 
    c(sum(abs(par)) - L1, sum(par^2) - L2)  
  }
  
  if(L1 <1 || L2<1) {
    searchSpace = max(L1, L2^2)
  } else { 
    searchSpace = min(L1, L2^2)
  }
  
  # cores=detectCores()
  cl <- makeCluster(k) 
  registerDoParallel(cl)
  
  out = foreach(i = 1:k, .export=ls(envir=globalenv()), .packages = c("DEoptimR"), .combine = rbind) %dopar% {
    try({
      validate <- flds[paste("Fold", i, sep="")] 
      validate <- unlist(validate, use.names=FALSE)
      train <- setdiff(1:n, validate)
      dft <- data[train,]
      
      set.seed(1234)
      theta <- JDEoptim(rep(-searchSpace, nCat), rep(searchSpace, nCat), fn=meanUOptim, constr=consEN, maxiter=iter, trace = TRUE, triter = 1)
      return(theta$par)
    })
  }
  stopCluster(cl)
  
  #write.csv(file="out.csv", x=out, row.names=FALSE)
  
  # Write the thetas in out to data
  for(i in 1:k) { 
    validate <- flds[paste("Fold", i, sep="")] 
    validate <- unlist(validate, use.names=FALSE)
    
    for(t in validate) { 
      data[t, match("UIP", colnames(data)):(match("UIP", colnames(data))+nCat-1)] <- as.numeric(out[i,])
    }
  }
  
  # Compute PPP weights, returns and utilities
  data$rPPP <- data$u <- 0
  
  for(t in 1:nrow(data)) { 
    # weights
    data[t, (5+nCountry+nCountry*nCat+nCat):(4+nCountry+nCountry*nCat+nCat+nCountry)] <- vecNt(data[t,2]) + (1/data[t,2])* t(matrix(as.vector(as.numeric(data[t, match("UIP", colnames(data)):(match("UIP", colnames(data))+nCat-1)])))) %*% matrix(as.numeric(data[t,(3+nCountry):(2+nCountry+nCat*nCountry)]), nrow=nCat)
    
    # returns
    data$rPPP[t] <- t(matrix(as.numeric(data[t, (5+nCountry+nCountry*nCat+nCat):(4+nCountry+nCountry*nCat+nCat+nCountry)]))) %*% matrix(as.vector(as.numeric(data[t, 3:(2+nCountry)])))
    
    # utility
    data$u[t] <- u( data$rPPP[t], gamma)
  }
  
  # Compute the ParaPP cumulative return
  data$ParaPP <- 1
  for(t in 2:nrow(data)) { 
    data$ParaPP[t] <- (1 + data$rPPP[t])*data$ParaPP[(t-1)]
  }
  
  # Diagnostics 
  data$L1 <- 0
  data$L2 <- 0
  
  for(t in 1:nrow(data)) { 
    data$L1[t] <- sum(abs(data[t, match("UIP", colnames(data)):(match("UIP", colnames(data))+nCat-1)]))
    data$L2[t] <- sum(data[t, match("UIP", colnames(data)):(match("UIP", colnames(data))+nCat-1)]^2)
  }
  
  # Sharp ratio
  meanR <- mean(data$rPPP[1:nrow(data)])*4
  sdR <- sd(data$rPPP[1:nrow(data)])*4
  SR <- meanR / sdR
  
  # Write to disk
  write.csv(file=paste("CrossVal ", "k=", k, " L1=", L1, " L2=", L2, " iter=", iter, " meanR= ", round(meanR,digits=3), " sdR=", round(sdR,digits=3), " SR= ", round(SR, digits = 3), ".csv", sep=""), x=data, row.names=FALSE)
  
  return(SR)
}

n <- nrow(data)

flds <- createFolds(1:n, k = k, list = TRUE, returnTrain = FALSE)


if(exists("LRange")) { 
  # Construct the dataframe of Sharpe Ratios
  ENCrossVal <- data.frame(matrix(0, length(LRange), length(LRange)))
  rownames(ENCrossVal) <- paste("L1=", LRange, sep="")
  colnames(ENCrossVal) <- paste("L2=", LRange, sep="")
  
  x <- 1
  for(L in LRange) {
    print(c("L1=", L, " L2=", L))
    ENCrossVal[x,x] <- crossVal(L,L)
    write.csv(file="crossValResultTable.csv", x=ENCrossVal)
    x <- x+1
  }
  
} else { 
  
  # Construct the dataframe of Sharpe Ratios
  ENCrossVal <- data.frame(matrix(0, length(L1Range), length(L2Range)))
  rownames(ENCrossVal) <- paste("L1=", L1Range, sep="")
  colnames(ENCrossVal) <- paste("L2=", L2Range, sep="")
  
  x <- 1
  y <- 1
  for(L1 in L1Range) {
    y <- 1
    for(L2 in L2Range) {
      print(c("L1=", L1, " L2=", L2))
      ENCrossVal[x,y] <- crossVal(L1,L2)
      write.csv(file="crossValResultTable.csv", x=ENCrossVal)
    
      y <- y+1
    }
    x <- x+1
  }

}
