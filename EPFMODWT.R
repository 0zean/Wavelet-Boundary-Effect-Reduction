library(wmtsa)
library(ggplot2)
library(quantmod)
library(plotly)
library(earth)
setwd("C:/Users/Nick/Desktop/Market")
set.seed(123)

# get stock data from yahoo-finance
#symbol <- "SPY"  
#series <- new.env()
#getSymbols.yahoo(symbol, env = series, from="2009-01-01", to=Sys.Date() + 1, auto.assign=T) 
#stock <- series[[symbol]]
#stock <- stock[, "SPY.Close"]

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


test <- signal[(length(signal)-99):length(signal)]
test2 <- vect[(length(vect)-99):length(vect)]

par(mfrow=c(2,1)) 
plot.ts(test)
plot.ts(test2)


src <- vect
src <- embed(src, 5)
src <- tail(src, 16000)
src <- data.frame(src)

ss <- tail(signal, 16000)

lbls = data.frame(ss)
lbls_dt = seq(nrow(lbls)*.7)
trnLbls <- lbls[lbls_dt, ]
chkLbls <- lbls[-lbls_dt, ]
  
op <- tail(log(esdf["Open"]), 16000)
  
data <- src[,-1]
data <- data.frame(data)
data$open <- op
data <- as.matrix(data)
  
data_dt = seq(nrow(data)*.7)
trnData <- data[data_dt, ]
chkData <- data[-data_dt, ]

train <- data.frame(trnLbls, trnData)
trainRow <- sample(nrow(train), replace = FALSE)
train <- train[trainRow, ]

t.lbls <- train[,1,drop=FALSE]
t.data <- train[,-1]

filter <- earth(x=t.data, y=t.lbls, degree=7)

pred <- predict(filter, trnData)


data.train <- data.frame(pred)
data.train$actual <- trnLbls

plot_ly(data.train, y = ~trnLbls, name = 'Predicted', type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~actual, name = 'Actual', mode = 'lines+markers') %>%
  layout(title = "EPF-MARS (In Sample)",
         xaxis = list(title = "Time-Step (5 mins)"),
         yaxis = list (title = "Log Price"))
