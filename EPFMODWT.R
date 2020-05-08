library(wmtsa)
library(ggplot2)
library(quantmod)
library(plotly)
library(earth)

setwd("path/to/csv")
set.seed(123)

esdf <- read.csv("emini.csv")
es <- esdf["Close"]

signal = as.ts(log(es))
ts.plot(signal)

# End-point flattening fuction
EPF <- function(data) {
  a = data[1]
  b = (data[length(data)] - a) / (length(data)-1)
  y <- vector(length = length(data))
  for (i in 1:length(data)) {
    y[i] <- data[i] - (a + b*(i-1))
  }
  return(y)
}


flat <- EPF(signal)
ts.plot(as.ts(flat))


# sliding window wavelet denoise saving last 2 coeffs from each window
# ep is last coeff epk is second to last
slideFunct <- function(data, window, step){
  total <- length(data)
  spots <- seq(from=1, to=(total-window), by=step)
  ep <- vector(length = total-window)
  epk <- vector(length = total-window)
  
  for(i in 1:length(spots)){
    newdata <- data[spots[i]:(spots[i]+window)]
    ff <- EPF(newdata)
    wave <- wavShrink(ff, wavelet="s8", n.level=3, shrink.fun="soft", 
                      thresh.fun="universal", xform="modwt", reflect=FALSE)
    filt <- as.ts(wave)
    ep[i] <- filt[length(filt)]
    epk[i] <- filt[length(filt)-1]
  df <- data.frame(ep, epk)
  }
  return(df)
}

vals = slideFunct(signal, 800, 1)
plot.ts(vals)


# sequentially sum the differences of endpoints from each sliding window in a recursive fashion
pre <- vals$ep - vals$epk
vect <- vector(length = 1)*0

for(i in 1:length(pre)){
  vect[i] <- pre[i] + vect[length(vect)]
}


# plot last 100 prices from the original signal and the filter
test <- signal[(length(signal)-99):length(signal)]
test2 <- vect[(length(vect)-99):length(vect)]

par(mfrow=c(2,1)) 
plot.ts(test)
plot.ts(test2)
