library(wmtsa)
library(smooth)
library(ggplot2)
library(quantmod)
library(plotly)
library(ggplot2)
library(alphavantager)


setwd("../Wavelet-Boundary-Effect-Reduction/")
set.seed(42)

# get stock data from alphavantage API
key <- readLines("key.txt") # read api key from text file
av_api_key(key)
data <- av_get("AAPL", av_fun = "TIME_SERIES_INTRADAY", interval = "5min", outputsize = 'full')

#signal = as.ts(log(es))
signal = rev(data$close)
ts.plot(signal)


# End-point flattening function
EPF <- function(data) {
  a = data[1]
  b = (data[length(data)] - a) / (length(data)-1)
  y <- vector(length = length(data))
  for (i in 1:length(data)) {
    y[i] <- data[i] - (a + b*(i-1))
  }
  return(y)
}


# plot of EPF on close price
signal <- as.ts(rev(data$close))
flat <- EPF(signal)
ts.plot(as.ts(flat))


data <- data.frame(
  day = seq(length(flat)),
  value = flat
)

p <- ggplot(data, aes(x=day, y=value)) +
  geom_line() + 
  xlab("")
ggplotly(p)


# sliding window wavelet transform saving last 2 coeffs from each window
# ep is last coeff epk is one before ep
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


# sequentially sum the differences of endpoints from each sliding window
pre <- vals$ep - vals$epk
vect <- vector(length = 1)*0

for(i in 1:length(pre)){
  vect[i] <- pre[i] + vect[length(vect)]
}

# 14 period MA of the filter
ma <-  SMA(vect, n=14)

price <- signal[(length(signal)-200):length(signal)]
epf_filter <- vect[(length(vect)-200):length(vect)]


# Ploting the filter and original Price series separately 
par(mfrow=c(2,1)) 
plot.ts(price)
plot.ts(epf_filter)

data2 <- data.frame(
  day = seq(length(price)),
  value1 = price,
  value2 = epf_filter
)


# Ploting the filter against the original Price series
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

ggplot(data2, aes(x=day)) +
  geom_line( aes(y=range01(value1))) + 
  geom_line( aes(y=range01(value2))) +
  scale_y_continuous(
    name = "Norm Values"
  )


# Plottig the filter with its 14 period MA
data3 <- data.frame(
  day = seq(201),
  value1 = vect[(length(vect)-200):length(vect)],
  value2 = ma[(length(ma)-200):length(ma)]
)

ggplot(data3, aes(x=day)) +
  geom_line( aes(y=value1)) + 
  geom_line( aes(y=value2)) +
  scale_y_continuous(
    name = "EPF"
  )
