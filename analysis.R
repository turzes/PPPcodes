library(readr)
library(DEoptimR)

set.seed(1234)
rm(list=ls())

winSize <- 90
 
df <- as.data.frame(read_csv("~/Dropbox/Francois2/PPP L1=2 L2=2 iter=500 lag=6 SR= 0.537 .csv"))

colnames(df)[113:123]

plot(df$TR, type="l")

colnames(df)[113:123]
df[(winSize+1):nrow(df),113:123]

allCoefs <- c()
for(i  in 113:123) {
  allCoefs <- c(allCoefs, df[(winSize+1):nrow(df),i])
}

plot(density(allCoefs))

plot(density(df[(winSize+1):nrow(df), "DC"]))


plot(((winSize+1):nrow(df)),df$ParaPP[(winSize+1):nrow(df)], type="l")


plot(((winSize+1):nrow(df)),df$rPPP[(winSize+1):nrow(df)], type="l")


plot(((winSize+1):nrow(df)),df$wEK[(winSize+1):nrow(df)], type="l")

colnames(df)[124:132]

for(t in (winSize+1):nrow(df)) { 
  print(sum(df[t, 124:132]))  
}


















